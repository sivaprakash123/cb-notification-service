package com.igot.cb.notification.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.igot.cb.authentication.util.AccessTokenValidator;
import com.igot.cb.notification.enums.NotificationReadStatus;
import com.igot.cb.notification.service.NotificationService;
import com.igot.cb.transactional.cassandrautils.CassandraOperation;
import com.igot.cb.util.Constants;
import com.igot.cb.util.TransformUtility;
import com.igot.cb.util.cache.CacheService;
import com.igot.cb.util.dto.SBApiResponse;
import com.igot.cb.util.exceptions.CustomException;
import io.micrometer.common.util.StringUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;


import java.time.*;
import java.util.*;
import java.util.stream.Collectors;

import static com.igot.cb.util.Constants.*;

@Service
@Slf4j
public class NotificationServiceImpl implements NotificationService {


    @Autowired
    AccessTokenValidator accessTokenValidator;

    @Autowired
    CassandraOperation cassandraOperation;

    @Autowired
    private TransformUtility transformUtility;

    @Autowired
    CacheService cacheService;

    @Autowired
    private ObjectMapper objectMapper;


    @Override
    public SBApiResponse createNotification(JsonNode userNotificationDetail, String token) {
        log.info("NotificationService::CreateNotification: inside the method");
        SBApiResponse response = transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_CREATE);

        try {

            Optional<String> userIdOptional = validateUser(token, response);
            if (userIdOptional.isEmpty()) {
                return response;
            }

            String userId = userIdOptional.get();
            log.info("UserId from auth token {}", userId);

            ZoneId zoneId = ZoneId.of(UTC);
            Instant instant = LocalDateTime.now().atZone(zoneId).toInstant();

            Map<String, Object> dbMap = new HashMap<>();
            dbMap.put(Constants.NOTIFICATION_ID, UUID.randomUUID().toString());
            dbMap.put(USER_ID, userId);
            dbMap.put(CREATED_AT, instant);
            dbMap.put(UPDATED_AT, instant);
            dbMap.put(IS_DELETED, false);
            dbMap.put(READ, false);
            dbMap.put(READ_AT, null);

            JsonNode requestNode = userNotificationDetail.get(REQUEST);
            if (ObjectUtils.isNotEmpty(requestNode) && requestNode.isObject()) {
                Iterator<Map.Entry<String, JsonNode>> fields = requestNode.fields();
                while (fields.hasNext()) {
                    Map.Entry<String, JsonNode> entry = fields.next();
                    dbMap.put(entry.getKey(), entry.getValue().asText());
                }
            } else {
                log.warn("Missing or invalid 'request' node in payload");
                response.getParams().setMsg("Missing or invalid 'request' node in payload");
                response.getParams().setStatus(Constants.FAILED);
                response.setResponseCode(HttpStatus.BAD_REQUEST);
                return response;
            }

            Object res = cassandraOperation.insertRecord(
                    Constants.KEYSPACE_SUNBIRD,
                    Constants.TABLE_USER_NOTIFICATION,
                    dbMap
            );
            log.info("Inserted notification: {}", res.toString());

            Map<String, Object> responseMap = new HashMap<>(dbMap);
            responseMap.remove(IS_DELETED);
            responseMap.remove(UPDATED_AT);
            responseMap.remove(USER_ID);
            responseMap.remove(READ_AT);

            response.setResponseCode(HttpStatus.OK);
            response.setResult(responseMap);

            String notificationId = (String) responseMap.get(NOTIFICATION_ID);
            if (StringUtils.isNotBlank(notificationId)) {
                cacheService.putCache("notification:" + notificationId, response);
            }

            return response;

        } catch (Exception e) {
            String errMsg = "Error while performing operation: " + e.getMessage();
            log.error(errMsg, e);
            response.getParams().setMsg(errMsg);
            response.getParams().setStatus(Constants.FAILED);
            response.setResponseCode(HttpStatus.INTERNAL_SERVER_ERROR);
            return response;
        }
    }


    @Override
    public SBApiResponse readByUserIdAndNotificationId(String notificationId, String token) {
        log.info("NotificationService::readByUserIdAndNotificationId: inside the method");
        SBApiResponse response = transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_READ_NOTIFICATIONID);

        try {
            Optional<String> userIdOptional = validateUser(token, response);
            if (!userIdOptional.isPresent()) {
                return response;
            }

            String userId = userIdOptional.get();

            List<Map<String, Object>> notifications = fetchNotifications(userId);

            Optional<Map<String, Object>> match = notifications.stream()
                    .filter(n -> notificationId.equals(n.get(NOTIFICATION_ID)))
                    .findFirst();

            if (match.isPresent()) {
                Map<String, Object> resultMap = new HashMap<>(match.get());
                resultMap.remove(IS_DELETED);
                resultMap.remove(UPDATED_AT);
                resultMap.remove(USER_ID);
                resultMap.remove(READ_AT);
                resultMap.remove(TEMPLATE_ID);
                response.setResult(resultMap);
                response.setResponseCode(HttpStatus.OK);
            } else {
                response.getParams().setMsg("Notification not found for this user.");
                response.getParams().setStatus(Constants.SUCCESS);
                response.setResponseCode(HttpStatus.OK);
            }

            return response;

        } catch (Exception e) {
            log.error("Error while processing notification read", e);
            throw new CustomException(Constants.ERROR, e.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }


    @Override
    public SBApiResponse readByUserIdAndLastXDaysNotifications(String token, int days, int page, int size, NotificationReadStatus status) {
        log.info("NotificationService::readByUserIdAndLastXDaysNotifications: inside the method");
        SBApiResponse response = transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_READ_N_DAYSID);

        try {
            Optional<String> userIdOptional = validateUser(token, response);
            if (userIdOptional.isEmpty()) {
                return response;
            }

            String userId = userIdOptional.get();
            log.info("UserId from auth token {}", userId);

            Instant fromDate = ZonedDateTime.now(ZoneOffset.UTC).minusDays(days).toInstant();


            List<Map<String, Object>> userNotificationList = cassandraOperation.getRecordsByPropertiesWithoutFiltering(
                    Constants.KEYSPACE_SUNBIRD,
                    Constants.TABLE_USER_NOTIFICATION,
                    Map.of(USER_ID, userId),
                    List.of(NOTIFICATION_ID, CREATED_AT, TYPE, MESSAGE, READ, ROLE, SOURCE, CATEGORY),
                    MAX_NOTIFICATIONS_FETCH_FOR_READ
            );


            List<Map<String, Object>> filteredNotifications = userNotificationList.stream()
                    .filter(notification -> {
                        Instant createdAt = (Instant) notification.get(CREATED_AT);

                        if (!ObjectUtils.allNotNull(createdAt, fromDate) || createdAt.isBefore(fromDate)) {
                            return false;
                        }

                        Boolean isRead = (Boolean) notification.get(READ);
                        if (status == NotificationReadStatus.READ) {
                            return Boolean.TRUE.equals(isRead);
                        } else if (status == NotificationReadStatus.UNREAD) {
                            return Boolean.FALSE.equals(isRead);
                        }
                        return true;
                    })
                    .collect(Collectors.toList());


            int total = filteredNotifications.size();
            int fromIndex = Math.min(page * size, total);
            int toIndex = Math.min(fromIndex + size, total);
            List<Map<String, Object>> paginated = filteredNotifications.subList(fromIndex, toIndex);


            Map<String, Object> resultMap = new HashMap<>();
            resultMap.put(NOTIFICATIONS, paginated);
            resultMap.put(TOTAL, total);
            resultMap.put(PAGE, page);
            resultMap.put(SIZE, size);
            resultMap.put(HAS_NEXT_PAGE, toIndex < total);

            response.setResponseCode(HttpStatus.OK);
            response.setResult(resultMap);
            return response;

        } catch (Exception e) {
            log.error("Error while processing readByUserIdAndLastXDaysNotifications", e);
            throw new CustomException(Constants.ERROR, e.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    @Override
    public SBApiResponse markNotificationsAsRead(String token, List<String> notificationIds) {
        log.info("NotificationService::markNotificationsAsRead - ids: {}", notificationIds);

        SBApiResponse response = transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_READ_UPDATEID);

        Optional<String> userIdOptional = validateUser(token, response);
        if (userIdOptional.isEmpty()) {
            return response;
        }

        String userId = userIdOptional.get();
        log.info("UserId from auth token {}", userId);

        String errMsg = validateNotificationReadRequest(notificationIds, response);
        if (StringUtils.isNotBlank(errMsg)) {
            return response;
        }

        List<Map<String, Object>> updated = new ArrayList<>();

        try {
            // Fetch user notifications
            List<Map<String, Object>> userNotifications = fetchNotifications(userId);

            for (String notificationId : notificationIds) {
                Optional<Map<String, Object>> match = userNotifications.stream()
                        .filter(r -> notificationId.equals(r.get(NOTIFICATION_ID)))
                        .findFirst();

                if (match.isPresent()) {
                    Map<String, Object> notification = match.get();
                    Instant createdAt = (Instant) notification.get(CREATED_AT);
                    Object readValue = notification.get(READ);

                    boolean isCreatedAtPresent = ObjectUtils.isNotEmpty(createdAt);
                    boolean isAlreadyRead = BooleanUtils.toBoolean((Boolean) readValue);

                    // Proceed only if the notification is not read and createdAt exists
                    if (isCreatedAtPresent && !isAlreadyRead) {
                        Instant readAt = Instant.now();

                        Map<String, Object> updateMap = Map.of(
                                READ, true,
                                READ_AT, readAt
                        );

                        // Use updateNotification method to perform the update
                        Map<String, Object> result = updateNotification(userId, notificationId, updateMap);

                        // If update is successful, add it to the updated list
                        if (Constants.SUCCESS.equalsIgnoreCase((String) result.get(Constants.RESPONSE))) {
                            updated.add(Map.of(
                                    ID, notificationId,
                                    READ, true,
                                    READ_AT, readAt.toString()
                            ));
                        }
                    } else {
                        log.info("Notification {} already marked as read. Skipping.", notificationId);
                    }
                }
            }

            response.getParams().setMsg("Notifications updated successfully");
            response.getParams().setStatus(Constants.SUCCESS);
            response.setResponseCode(HttpStatus.OK);
            response.setResult(Map.of("notifications", updated));
            return response;

        } catch (Exception e) {
            log.error("Error in markNotificationsAsRead", e);
            updateErrorDetails(response, HttpStatus.INTERNAL_SERVER_ERROR, "Error while updating notifications.");
            return response;
        }
    }


    @Override
    public SBApiResponse markNotificationsAsDeleted(String token, List<String> notificationIds) {
        log.info("NotificationService::markNotificationsAsDeleted - ids: {}", notificationIds);

        SBApiResponse response = transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_DELETE);
        List<Map<String, Object>> updated = new ArrayList<>();

        try {
            Optional<String> userIdOptional = validateUser(token, response);
            if (userIdOptional.isEmpty()) {
                return response;
            }

            String userId = userIdOptional.get();
            log.info("UserId from auth token {}", userId);


            String errMsg = validateNotificationReadRequest(notificationIds, response);
            if (StringUtils.isNotBlank(errMsg)) {
                return response;
            }


            for (String notificationId : notificationIds) {
                Map<String, Object> updateMap = Map.of(
                        IS_DELETED, true,
                        UPDATED_AT, Instant.now()
                );

                Map<String, Object> result = updateNotification(userId, notificationId, updateMap);


                if (Constants.SUCCESS.equalsIgnoreCase((String) result.get(Constants.RESPONSE))) {
                    updated.add(Map.of(
                            ID, notificationId,
                            IS_DELETED, true
                    ));
                } else {
                    log.info("Notification {} is already marked as deleted or has no created_at", notificationId);
                }
            }

            response.getParams().setMsg("Notifications marked as deleted successfully");
            response.getParams().setStatus(Constants.SUCCESS);
            response.setResponseCode(HttpStatus.OK);
            response.setResult(Map.of(NOTIFICATIONS, updated));

            return response;

        } catch (Exception e) {
            log.error("Error in markNotificationsAsDeleted", e);
            throw new CustomException(Constants.ERROR, e.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    private String validateNotificationReadRequest(List<String> ids, SBApiResponse response) {
        if (CollectionUtils.isEmpty(ids)) {
            response.getParams().setStatus(Constants.FAILED);
            response.getParams().setMsg("Request must contain a non-empty list of notification IDs.");
            response.setResponseCode(HttpStatus.BAD_REQUEST);
            return "Request must contain a non-empty list of notification IDs.";
        }

        if (ids.size() > Constants.MAX_NOTIFICATION_READ_BATCH_SIZE) {
            response.getParams().setStatus(Constants.FAILED);
            response.getParams().setMsg("You can only mark up to " + Constants.MAX_NOTIFICATION_READ_BATCH_SIZE + " notifications as read at a time.");
            response.setResponseCode(HttpStatus.BAD_REQUEST);
            return "You can only mark up to " + Constants.MAX_NOTIFICATION_READ_BATCH_SIZE + " notifications as read at a time.";
        }

        return "";
    }

    private void updateErrorDetails(SBApiResponse response, HttpStatus status, String message) {
        response.getParams().setStatus(Constants.FAILED);
        response.getParams().setMsg(message);
        response.setResponseCode(status);
    }

    private List<Map<String, Object>> fetchNotifications(String userId) {
        Map<String, Object> queryMap = new HashMap<>();
        queryMap.put(USER_ID, userId);

        return cassandraOperation.getRecordsByPropertiesWithoutFiltering(
                Constants.KEYSPACE_SUNBIRD,
                Constants.TABLE_USER_NOTIFICATION,
                queryMap,
                null,
                MAX_NOTIFICATIONS_FETCH_FOR_READ
        );
    }

    private Optional<String> validateUser(String token, SBApiResponse response) {
        String userId = accessTokenValidator.verifyUserToken(token);
        if (StringUtils.isBlank(userId) || userId.equalsIgnoreCase(Constants.UNAUTHORIZED)) {
            response.getParams().setMsg(Constants.USER_ID_DOESNT_EXIST);
            response.getParams().setStatus(Constants.FAILED);
            response.setResponseCode(HttpStatus.BAD_REQUEST);
            return Optional.empty();
        }
        return Optional.of(userId);
    }

    private Map<String, Object> updateNotification(String userId, String notificationId, Map<String, Object> updateMap) {
        List<Map<String, Object>> records = fetchNotifications(userId);

        Optional<Map<String, Object>> match = records.stream()
                .filter(r -> notificationId.equals(r.get(NOTIFICATION_ID)))
                .findFirst();

        if (match.isPresent()) {
            Map<String, Object> notification = match.get();
            Instant createdAt = (Instant) notification.get(CREATED_AT);

            Map<String, Object> compositeKey = Map.of(
                    USER_ID, userId,
                    CREATED_AT, createdAt
            );

            return cassandraOperation.updateRecord(
                    Constants.KEYSPACE_SUNBIRD,
                    Constants.TABLE_USER_NOTIFICATION,
                    updateMap,
                    compositeKey
            );
        }
        return Collections.emptyMap();
    }


}
