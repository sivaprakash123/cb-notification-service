package com.igot.cb.notification.service.impl;

import com.datastax.oss.driver.api.core.CqlSession;
import com.datastax.oss.driver.api.core.cql.BoundStatement;
import com.datastax.oss.driver.api.core.cql.PreparedStatement;
import com.datastax.oss.driver.api.core.cql.ResultSet;
import com.fasterxml.jackson.databind.JsonNode;
import com.igot.cb.authentication.util.AccessTokenValidator;
//import com.igot.cb.cache.RedisCacheMgr;
import com.igot.cb.notification.service.NotificationService;
import com.igot.cb.transactional.cassandrautils.CassandraOperation;
import com.igot.cb.util.Constants;
import com.igot.cb.util.TransformUtility;
import com.igot.cb.util.dto.SBApiResponse;
import com.igot.cb.util.exceptions.CustomException;
import io.micrometer.common.util.StringUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;


import java.time.*;
import java.util.*;
import java.util.stream.Collectors;

@Service
@Slf4j
public class NotificationServiceImpl implements NotificationService {

    @Autowired
    AccessTokenValidator accessTokenValidator;

    @Autowired
    CassandraOperation cassandraOperation;

    @Autowired
    private TransformUtility transformUtility;

//    @Autowired
//    RedisCacheMgr redisCacheMgr;
//

    @Override
    public SBApiResponse createNotification(JsonNode userNotificationDetail, String token) {
        log.info("NotificationService::CreateNotification:inside the method");
        SBApiResponse response = transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_CREATE);
        try {
//           String userId = accessTokenValidator.verifyUserToken(token);
            String userId ="96102";
            log.info("UserId from auth token {}", userId);
            if (StringUtils.isBlank(userId) || userId.equalsIgnoreCase(Constants.UNAUTHORIZED)) {
                response.getParams().setMsg(Constants.USER_ID_DOESNT_EXIST);
                response.getParams().setStatus(Constants.FAILED);
                response.setResponseCode(HttpStatus.BAD_REQUEST);
                return response;
            }

                ZoneId zoneId = ZoneId.of("UTC");
                Instant instant = LocalDateTime.now().atZone(zoneId).toInstant();
                Map<String, Object> userCourseEnrollMap = new HashMap<>();
                userCourseEnrollMap.put("notification_id",UUID.randomUUID().toString());
                userCourseEnrollMap.put("user_id", userId);
                userCourseEnrollMap.put("type",
                        userNotificationDetail.get("type").asText());
                userCourseEnrollMap.put("category",
                        userNotificationDetail.get("category").asText());
                userCourseEnrollMap.put("source",
                        userNotificationDetail.get("source").asText());
                userCourseEnrollMap.put("template_id",
                        userNotificationDetail.get("category").asText());
                userCourseEnrollMap.put("role",
                        userNotificationDetail.get("role").asText());
            userCourseEnrollMap.put("message",
                    userNotificationDetail.get("message").asText());
            userCourseEnrollMap.put("created_at",
                    instant);
            userCourseEnrollMap.put("updated_at",
                    instant);
            userCourseEnrollMap.put("is_deleted",
                    false);
            userCourseEnrollMap.put("read",
                    false);
            userCourseEnrollMap.put("read_at",
                    null);
             Object res=   cassandraOperation.insertRecord(Constants.KEYSPACE_SUNBIRD,
                        Constants.TABLE_USER_NOTIFICATION, userCourseEnrollMap);
            System.out.println(res.toString()+"res");
                response.setResponseCode(HttpStatus.OK);
                response.setResult(userCourseEnrollMap);
                return response;
        } catch (Exception e) {
            String errMsg = "Error while performing operation." + e.getMessage();
            log.error(errMsg, e);
            response.getParams().setMsg(errMsg);
            response.getParams().setStatus(Constants.FAILED);
            response.setResponseCode(HttpStatus.INTERNAL_SERVER_ERROR);
        }

        return response;
    }

    @Override
    public SBApiResponse readByUserIdAndNotificationId(String notificationId, String token) {
        log.info("NotificationService::readByUserIdAndNotificationId:inside the method");
        SBApiResponse response = transformUtility.createDefaultResponse(Constants.CIOS_ENROLLMENT_READ_COURSEID);
        try {
//            String userId = accessTokenValidator.verifyUserToken(token);
            String userId ="96102";
            if (org.apache.commons.lang3.StringUtils.isBlank(userId) || userId.equalsIgnoreCase(Constants.UNAUTHORIZED)) {
                response.getParams().setMsg(Constants.USER_ID_DOESNT_EXIST);
                response.getParams().setStatus(Constants.FAILED);
                response.setResponseCode(HttpStatus.BAD_REQUEST);
                return response;
            }
            Map<String, Object> propertyMap = new HashMap<>();
            propertyMap.put("user_id", userId);
            propertyMap.put("notification_id", notificationId);
            List<Map<String, Object>> userNotificationList = cassandraOperation.getRecordsByPropertiesWithoutFiltering(
                    Constants.KEYSPACE_SUNBIRD,
                    Constants.TABLE_USER_NOTIFICATION,
                    propertyMap,
                    null,
                    1
            );
            System.out.println(userNotificationList+"hello");
            if (!userNotificationList.isEmpty()) {
                for (Map<String, Object> enrollment : userNotificationList) {
                    if (!enrollment.isEmpty()) {
                        response.setResponseCode(HttpStatus.OK);
                        response.setResult(enrollment);
                        //     cacheService.putCache(userId + courseid, response);
                    } else {
                        response.getParams().setMsg("notificationId is not matching");
                        response.getParams().setStatus(Constants.FAILED);
                        response.setResponseCode(HttpStatus.BAD_REQUEST);
                        return response;
                    }
                }
            } else {
                response.getParams().setMsg("Notification not Found");
                response.getParams().setStatus(Constants.SUCCESS);
                response.setResponseCode(HttpStatus.OK);
                return response;
            }
            return response;
        } catch (Exception e) {
            log.error("error while processing", e);
            throw new CustomException(Constants.ERROR, e.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    @Override
    public SBApiResponse readByUserIdAndLast30DaysNotifications(String token, int page, int size) {
        log.info("NotificationService::readByUserIdAndLast30DaysNotifications: inside the method");

        SBApiResponse response = transformUtility.createDefaultResponse(Constants.CIOS_ENROLLMENT_READ_COURSEID);

        try {
            // String userId = accessTokenValidator.verifyUserToken(token);
            String userId = "96102"; // Replace with actual user ID logic

            if (org.apache.commons.lang3.StringUtils.isBlank(userId) || userId.equalsIgnoreCase(Constants.UNAUTHORIZED)) {
                response.getParams().setMsg(Constants.USER_ID_DOESNT_EXIST);
                response.getParams().setStatus(Constants.FAILED);
                response.setResponseCode(HttpStatus.BAD_REQUEST);
                return response;
            }

            // Calculate 30 days ago
            ZonedDateTime thirtyDaysAgo = ZonedDateTime.now(ZoneOffset.UTC).minusDays(30);
            Instant fromDate = thirtyDaysAgo.toInstant();

            // Query notifications by user_id
            List<Map<String, Object>> userNotificationList = cassandraOperation.getRecordsByPropertiesWithoutFiltering(
                    Constants.KEYSPACE_SUNBIRD,
                    Constants.TABLE_USER_NOTIFICATION,
                    Map.of("user_id", userId),
                    List.of("notification_id", "created_at", "type", "message", "read"),
                    200 // Fetch more than page * size to support pagination
            );

            // Filter for last 30 days
            List<Map<String, Object>> filteredNotifications = userNotificationList.stream()
                    .filter(notification -> {
                        Instant createdAt = (Instant) notification.get("created_at");
                        return createdAt != null && createdAt.isAfter(fromDate);
                    })
                    .collect(Collectors.toList());

            // Pagination logic
            int total = filteredNotifications.size();
            int fromIndex = Math.min(page * size, total);
            int toIndex = Math.min(fromIndex + size, total);

            List<Map<String, Object>> paginated = filteredNotifications.subList(fromIndex, toIndex);

            Map<String, Object> resultMap = new HashMap<>();
            resultMap.put("notifications", paginated);
            resultMap.put("total", total);
            resultMap.put("page", page);
            resultMap.put("size", size);
            resultMap.put("hasNextPage", toIndex < total);

            response.setResponseCode(HttpStatus.OK);
            response.setResult(resultMap);

            return response;

        } catch (Exception e) {
            log.error("Error while processing readByUserIdAndLast30DaysNotifications", e);
            throw new CustomException(Constants.ERROR, e.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    @Override
    public SBApiResponse markNotificationsAsRead(String token, List<String> notificationIds) {
        log.info("NotificationService::markNotificationsAsRead - ids: {}", notificationIds);

        SBApiResponse response = transformUtility.createDefaultResponse("notification.read");
        List<Map<String, Object>> updated = new ArrayList<>();

        try {
            String userId = "96102"; // Replace with token-based logic

            for (String notificationId : notificationIds) {
                // Step 1: Fetch the notification to get created_at
                List<Map<String, Object>> records = cassandraOperation.getRecordsByPropertiesWithoutFiltering(
                        Constants.KEYSPACE_SUNBIRD,
                        Constants.TABLE_USER_NOTIFICATION,
                        Map.of("user_id", userId), // Fetch partition
                        null,
                        100
                );

                Optional<Map<String, Object>> match = records.stream()
                        .filter(r -> notificationId.equals(r.get("notification_id")))
                        .findFirst();

                if (match.isPresent()) {
                    Instant createdAt = (Instant) match.get().get("created_at");
                    if (createdAt != null) {
                        // Step 2: Prepare update
                        Instant readAt = Instant.now();
                        Map<String, Object> updateMap = Map.of(
                                "read", true,
                                "read_at", readAt
                        );

                        Map<String, Object> compositeKey = Map.of(
                                "user_id", userId,
                                "created_at", createdAt
                        );

                        Map<String, Object> result = cassandraOperation.updateRecord(
                                Constants.KEYSPACE_SUNBIRD,
                                Constants.TABLE_USER_NOTIFICATION,
                                updateMap,
                                compositeKey
                        );

                        if (Constants.SUCCESS.equalsIgnoreCase((String) result.get(Constants.RESPONSE))) {
                            updated.add(Map.of(
                                    "id", notificationId,
                                    "read", true,
                                    "read_at", readAt.toString()
                            ));
                        }
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
            throw new CustomException(Constants.ERROR, e.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }








}
