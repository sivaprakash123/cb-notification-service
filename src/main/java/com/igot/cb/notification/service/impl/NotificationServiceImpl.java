package com.igot.cb.notification.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.igot.cb.authentication.util.AccessTokenValidator;
import com.igot.cb.notification.enums.NotificationReadStatus;
import com.igot.cb.notification.service.NotificationService;
import com.igot.cb.transactional.cassandrautils.CassandraOperation;
import com.igot.cb.util.ApiResponse;
import com.igot.cb.util.Constants;
import com.igot.cb.util.ProjectUtil;
import io.micrometer.common.util.StringUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
    private ObjectMapper objectMapper;

    private final Logger logger = LoggerFactory.getLogger(NotificationServiceImpl.class);

    @Override
    public ApiResponse createNotification(JsonNode userNotificationDetail, String authToken) {
        log.info("NotificationService::createNotification: inside the method");
        ApiResponse outgoingResponse = ProjectUtil.createDefaultResponse(Constants.USER_NOTIFICATION_CREATE);

        try {
            String userId = accessTokenValidator.fetchUserIdFromAccessToken(authToken);
            if (StringUtils.isEmpty(userId)) {
                updateErrorDetails(outgoingResponse, Constants.USER_ID_DOESNT_EXIST, HttpStatus.BAD_REQUEST);
                return outgoingResponse;
            }

            ZoneId zoneId = ZoneId.of(UTC);
            Instant instant = LocalDateTime.now().atZone(zoneId).toInstant();

            Map<String, Object> dbMap = new HashMap<>();
            dbMap.put(Constants.NOTIFICATION_ID, java.util.UUID.randomUUID().toString());
            dbMap.put(Constants.USER_ID, userId);
            dbMap.put(Constants.CREATED_AT, instant);
            dbMap.put(Constants.UPDATED_AT, instant);
            dbMap.put(Constants.IS_DELETED, false);
            dbMap.put(Constants.READ, false);
            dbMap.put(Constants.READ_AT, null);

            JsonNode requestNode = userNotificationDetail.get(Constants.REQUEST);
            if (ObjectUtils.isNotEmpty(requestNode) && requestNode.isObject()) {
                Iterator<Map.Entry<String, JsonNode>> fields = requestNode.fields();
                while (fields.hasNext()) {
                    Map.Entry<String, JsonNode> entry = fields.next();
                    JsonNode valueNode = entry.getValue();
                    if (valueNode.isValueNode()) {
                        dbMap.put(entry.getKey(), valueNode.asText());
                    } else {
                        dbMap.put(entry.getKey(), valueNode.toString());
                    }
                }
            } else {
                log.warn("Missing or invalid 'request' node: {}", userNotificationDetail.toString());
                outgoingResponse.getParams().setErrMsg("Missing or invalid 'request' node in payload");
                outgoingResponse.getParams().setStatus(Constants.FAILED);
                outgoingResponse.setResponseCode(HttpStatus.BAD_REQUEST);
                return outgoingResponse;
            }

            Object res = cassandraOperation.insertRecord(
                    Constants.KEYSPACE_SUNBIRD,
                    Constants.TABLE_USER_NOTIFICATION,
                    dbMap
            );
            log.info("Inserted notification: {}", res.toString());

            Map<String, Object> responseMap = new HashMap<>(dbMap);
            Map<String, Object> resultMap = prepareNotificationResponse(responseMap);

            outgoingResponse.setResponseCode(HttpStatus.OK);
            outgoingResponse.setResult(resultMap);
            log.info("NotificationService::createNotification saved successfully");

        } catch (Exception e) {
            log.error("Error while saving notification to Cassandra: {}", e.getMessage(), e);
            updateErrorDetails(outgoingResponse, "Internal server error while saving notification data",
                    HttpStatus.INTERNAL_SERVER_ERROR);
        }

        return outgoingResponse;
    }


    @Override
    public ApiResponse readByUserIdAndNotificationId(String notificationId, String authToken) {
        log.info("NotificationService::readByUserIdAndNotificationId: inside the method");
        ApiResponse outgoingResponse = ProjectUtil.createDefaultResponse(Constants.USER_NOTIFICATION_READ_NOTIFICATIONID);

        try {
            String userId = accessTokenValidator.fetchUserIdFromAccessToken(authToken);
            if (StringUtils.isEmpty(userId)) {
                updateErrorDetails(outgoingResponse, Constants.USER_ID_DOESNT_EXIST, HttpStatus.BAD_REQUEST);
                return outgoingResponse;
            }

            List<Map<String, Object>> notifications = fetchNotifications(userId);

            Optional<Map<String, Object>> match = notifications.stream()
                    .filter(n -> notificationId.equals(n.get(NOTIFICATION_ID)))
                    .findFirst();

            if (match.isPresent()) {
                Map<String, Object> resultMap = prepareNotificationResponse(match.get());
                outgoingResponse.setResult(resultMap);
                outgoingResponse.setResponseCode(HttpStatus.OK);
            } else {
                outgoingResponse.getParams().setErrMsg("Notification not found for this user.");
                outgoingResponse.getParams().setStatus(Constants.SUCCESS);
                outgoingResponse.setResponseCode(HttpStatus.OK);
            }
            logger.info("NotificationServiceImpl::readByUserIdAndNotificationId retrieved successfully ");

        } catch (Exception e) {
            logger.error("Error while fetching readByUserIdAndNotificationId from Cassandra: {}", e.getMessage(), e);
            updateErrorDetails(outgoingResponse, "Internal server error while fetching notification by userId",
                    HttpStatus.INTERNAL_SERVER_ERROR);
        }
        return outgoingResponse;
    }

    @Override
    public ApiResponse readByUserIdAndLastXDaysNotifications(String authToken, int days, int page, int size, NotificationReadStatus status) {
        log.info("NotificationService::readByUserIdAndLastXDaysNotifications: inside the method");
        ApiResponse outgoingResponse = ProjectUtil.createDefaultResponse(Constants.USER_NOTIFICATION_READ_N_DAYSID);

        try {
            String userId = accessTokenValidator.fetchUserIdFromAccessToken(authToken);
            if (StringUtils.isEmpty(userId)) {
                updateErrorDetails(outgoingResponse, Constants.USER_ID_DOESNT_EXIST, HttpStatus.BAD_REQUEST);
                return outgoingResponse;
            }

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
                    .toList();

            int total = filteredNotifications.size();
            int fromIndex = Math.min(page * size, total);
            int toIndex = Math.min(fromIndex + size, total);
            List<Map<String, Object>> paginated = filteredNotifications.subList(fromIndex, toIndex);

            List<Map<String, Object>> processed = paginated.stream()
                    .map(this::prepareNotificationResponse)
                    .collect(Collectors.toList());

            Map<String, Object> resultMap = new HashMap<>();
            resultMap.put(NOTIFICATIONS, processed);
            resultMap.put(TOTAL, total);
            resultMap.put(PAGE, page);
            resultMap.put(SIZE, size);
            resultMap.put(HAS_NEXT_PAGE, toIndex < total);

            outgoingResponse.setResponseCode(HttpStatus.OK);
            outgoingResponse.setResult(resultMap);
            logger.info("NotificationServiceImpl::readByUserIdAndLastXDaysNotifications list retrieved successfully");

        } catch (Exception e) {
            logger.error("Error while fetching readByUserIdAndLastXDaysNotifications from Cassandra: {}", e.getMessage(), e);
            updateErrorDetails(outgoingResponse,
                    "Internal server error while fetching readByUserIdAndLastXDaysNotifications list",
                    HttpStatus.INTERNAL_SERVER_ERROR);
        }

        return outgoingResponse;
    }


    @Override
    public ApiResponse markNotificationsAsRead(String authToken, List<String> notificationIds) {
        log.info("NotificationService::markNotificationsAsRead - ids: {}", notificationIds);

        ApiResponse outgoingResponse = ProjectUtil.createDefaultResponse(Constants.USER_NOTIFICATION_READ_UPDATEID);

        String userId = accessTokenValidator.fetchUserIdFromAccessToken(authToken);
        if (StringUtils.isEmpty(userId)) {
            updateErrorDetails(outgoingResponse, Constants.USER_ID_DOESNT_EXIST, HttpStatus.BAD_REQUEST);
            return outgoingResponse;
        }

        String errMsg = validateNotificationReadRequest(notificationIds, outgoingResponse);
        if (StringUtils.isNotBlank(errMsg)) {
            return outgoingResponse;
        }

        List<Map<String, Object>> updated = new ArrayList<>();

        try {
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
                    boolean isAlreadyRead = org.apache.commons.lang3.BooleanUtils.toBoolean((Boolean) readValue);

                    if (isCreatedAtPresent && !isAlreadyRead) {
                        Instant readAt = Instant.now();

                        Map<String, Object> updateMap = Map.of(
                                READ, true,
                                READ_AT, readAt
                        );


                        Map<String, Object> result = updateNotification(userId, notificationId, updateMap);


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

            outgoingResponse.getParams().setErrMsg("Notifications updated successfully");
            outgoingResponse.getParams().setStatus(Constants.SUCCESS);
            outgoingResponse.setResponseCode(HttpStatus.OK);
            outgoingResponse.setResult(Map.of("notifications", updated));
            logger.info("NotificationServiceImpl::markNotificationsAsRead  update successfully ");

        } catch (Exception e) {
            logger.error("Error while fetching  markNotificationsAsRead update from Cassandra: {}", e.getMessage(), e);
            updateErrorDetails(outgoingResponse, "Internal server error while fetching markNotificationsAsRead  update",
                    HttpStatus.INTERNAL_SERVER_ERROR);
            return outgoingResponse;
        }
        return outgoingResponse;
    }


    @Override
    public ApiResponse markNotificationsAsDeleted(String authToken, List<String> notificationIds) {
        log.info("NotificationService::markNotificationsAsDeleted - ids: {}", notificationIds);

        ApiResponse outgoingResponse = ProjectUtil.createDefaultResponse(Constants.USER_NOTIFICATION_DELETE);
        List<Map<String, Object>> updated = new ArrayList<>();

        try {
            String userId = accessTokenValidator.fetchUserIdFromAccessToken(authToken);
            if (StringUtils.isEmpty(userId)) {
                updateErrorDetails(outgoingResponse, Constants.USER_ID_DOESNT_EXIST, HttpStatus.BAD_REQUEST);
                return outgoingResponse;
            }

            String errMsg = validateNotificationReadRequest(notificationIds, outgoingResponse);
            if (StringUtils.isNotBlank(errMsg)) {
                return outgoingResponse;
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

            outgoingResponse.getParams().setErrMsg("Notifications marked as deleted successfully");
            outgoingResponse.getParams().setStatus(Constants.SUCCESS);
            outgoingResponse.setResponseCode(HttpStatus.OK);
            outgoingResponse.setResult(Map.of(NOTIFICATIONS, updated));

            logger.info("NotificationServiceImpl::markNotificationsAsDeleted  delete successfully ");

        } catch (Exception e) {
            logger.error("Error while fetching  markNotificationsAsDeleted delete from Cassandra: {}", e.getMessage(), e);
            updateErrorDetails(outgoingResponse, "Internal server error while fetching markNotificationsAsDeleted  delete",
                    HttpStatus.INTERNAL_SERVER_ERROR);
            return outgoingResponse;
        }
        return outgoingResponse;
    }

    private String validateNotificationReadRequest(List<String> ids, ApiResponse response) {
        if (org.springframework.util.CollectionUtils.isEmpty(ids)) {
            response.getParams().setStatus(Constants.FAILED);
            response.getParams().setErrMsg("Request must contain a non-empty list of notification IDs.");
            response.setResponseCode(HttpStatus.BAD_REQUEST);
            return "Request must contain a non-empty list of notification IDs.";
        }

        if (ids.size() > Constants.MAX_NOTIFICATION_READ_BATCH_SIZE) {
            response.getParams().setStatus(Constants.FAILED);
            response.getParams().setErrMsg("You can only mark up to " + Constants.MAX_NOTIFICATION_READ_BATCH_SIZE + " notifications as read at a time.");
            response.setResponseCode(HttpStatus.BAD_REQUEST);
            return "You can only mark up to " + Constants.MAX_NOTIFICATION_READ_BATCH_SIZE + " notifications as read at a time.";
        }

        return "";
    }

    private void updateErrorDetails(ApiResponse response, String errorMessage, HttpStatus httpStatus) {
        response.getParams().setStatus(Constants.FAILED);
        response.getParams().setErrMsg(errorMessage);
        response.setResponseCode(httpStatus);
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

            return cassandraOperation.updateRecordByCompositeKey(
                    Constants.KEYSPACE_SUNBIRD,
                    Constants.TABLE_USER_NOTIFICATION,
                    updateMap,
                    compositeKey
            );
        }
        return Collections.emptyMap();
    }


    public Map<String, Object> prepareNotificationResponse(Map<String, Object> dbRecord) {
        Map<String, Object> resultMap = new HashMap<>(dbRecord);
        List<String> fieldsToRemove = Arrays.asList(
                Constants.IS_DELETED,
                Constants.UPDATED_AT,
                Constants.USER_ID,
                Constants.READ_AT,
                Constants.TEMPLATE_ID
        );
        fieldsToRemove.forEach(resultMap::remove);
        Object messageObj = resultMap.get("message");

        if (messageObj instanceof String && messageObj != null) {
            try {
                JsonNode parsed = objectMapper.readTree((String) messageObj);
                resultMap.put("message", parsed);
                log.info("Message successfully parsed into JSON: {}", parsed.toPrettyString());
            } catch (Exception e) {
                log.warn("Could not parse message field as JSON: {}", e.getMessage());
            }
        } else {
            log.warn("Message field is not a valid string or is null.");
        }

        return resultMap;
    }

}
