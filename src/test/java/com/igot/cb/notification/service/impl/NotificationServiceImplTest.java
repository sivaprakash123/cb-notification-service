package com.igot.cb.notification.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.igot.cb.authentication.util.AccessTokenValidator;
import com.igot.cb.notification.enums.NotificationReadStatus;
import com.igot.cb.transactional.cassandrautils.CassandraOperation;
import com.igot.cb.util.Constants;
import com.igot.cb.util.TransformUtility;
import com.igot.cb.util.cache.CacheService;
import com.igot.cb.util.dto.SBApiResponse;
import com.igot.cb.util.dto.SunbirdApiRespParam;
import com.igot.cb.util.exceptions.CustomException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.*;
import org.springframework.http.HttpStatus;

import java.time.Instant;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class NotificationServiceImplTest {

    @InjectMocks
    private NotificationServiceImpl notificationService;

    @Mock
    private AccessTokenValidator accessTokenValidator;

    @Mock
    private CassandraOperation cassandraOperation;

    @Mock
    private TransformUtility transformUtility;

    @Mock
    private CacheService cacheService;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testCreateNotification_Success() {
        String token = "valid_token";
        String userId = "user123";

        ObjectNode requestPayload = objectMapper.createObjectNode();
        requestPayload.put("type", "INFO");
        requestPayload.put("message", "Hello, World!");

        ObjectNode mainPayload = objectMapper.createObjectNode();
        mainPayload.set("request", requestPayload);

        SBApiResponse mockResponse = new SBApiResponse(Constants.USER_NOTIFICATION_CREATE);
        mockResponse.setParams(new SunbirdApiRespParam("msg123"));

        when(accessTokenValidator.verifyUserToken(token)).thenReturn(userId);
        when(transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_CREATE)).thenReturn(mockResponse);
        when(cassandraOperation.insertRecord(any(), any(), any())).thenReturn(new HashMap<>());

        SBApiResponse response = notificationService.createNotification(mainPayload, token);

        assertEquals(HttpStatus.OK, response.getResponseCode());
        assertNotNull(response.getResult());
        assertTrue(response.getResult().containsKey(Constants.NOTIFICATION_ID));
        verify(cacheService, times(1)).putCache(startsWith("notification:"), eq(response));
    }

    @Test
    void testReadByUserIdAndNotificationId_Found() {
        String token = "valid_token";
        String userId = "user123";
        String notificationId = "notif-001";

        Map<String, Object> notification = new HashMap<>();
        notification.put(Constants.NOTIFICATION_ID, notificationId);
        notification.put(Constants.USER_ID, userId);
        notification.put(Constants.TYPE, "INFO");
        notification.put(Constants.MESSAGE, "Test message");
        notification.put(Constants.CREATED_AT, Instant.now());
        notification.put(Constants.READ, false);
        notification.put(Constants.UPDATED_AT, Instant.now());
        notification.put(Constants.IS_DELETED, false);
        notification.put(Constants.READ_AT, null);
        notification.put(Constants.TEMPLATE_ID, "template-xyz");

        List<Map<String, Object>> notifications = List.of(notification);

        SBApiResponse mockResponse = new SBApiResponse(Constants.USER_NOTIFICATION_READ_NOTIFICATIONID);
        mockResponse.setParams(new SunbirdApiRespParam("msg-001"));

        when(transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_READ_NOTIFICATIONID)).thenReturn(mockResponse);
        when(accessTokenValidator.verifyUserToken(token)).thenReturn(userId);
        when(cassandraOperation.getRecordsByPropertiesWithoutFiltering(any(), any(), any(), any(), anyInt()))
                .thenReturn(notifications);


        SBApiResponse response = notificationService.readByUserIdAndNotificationId(notificationId, token);


        assertEquals(HttpStatus.OK, response.getResponseCode());
        assertNotNull(response.getResult());
        assertEquals(notificationId, response.getResult().get(Constants.NOTIFICATION_ID));
        assertFalse(response.getResult().containsKey(Constants.IS_DELETED));
        assertFalse(response.getResult().containsKey(Constants.UPDATED_AT));
        assertFalse(response.getResult().containsKey(Constants.USER_ID));
        assertFalse(response.getResult().containsKey(Constants.READ_AT));
        assertFalse(response.getResult().containsKey(Constants.TEMPLATE_ID));
    }

    @Test
    void testReadByUserIdAndNotificationId_NotFound() {
        String token = "valid_token";
        String userId = "user123";
        String notificationId = "missing-id";

        SBApiResponse mockResponse = new SBApiResponse(Constants.USER_NOTIFICATION_READ_NOTIFICATIONID);
        mockResponse.setParams(new SunbirdApiRespParam("msg-002"));

        when(transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_READ_NOTIFICATIONID)).thenReturn(mockResponse);
        when(accessTokenValidator.verifyUserToken(token)).thenReturn(userId);
        when(cassandraOperation.getRecordsByPropertiesWithoutFiltering(any(), any(), any(), any(), anyInt()))
                .thenReturn(Collections.emptyList());

        SBApiResponse response = notificationService.readByUserIdAndNotificationId(notificationId, token);


        assertEquals(HttpStatus.OK, response.getResponseCode());
        assertEquals("Notification not found for this user.", response.getParams().getMsg());
    }

    @Test
    void testReadByUserIdAndLastXDaysNotifications_Success() {
        String token = "valid_token";
        String userId = "user123";
        int days = 5, page = 0, size = 2;

        SBApiResponse mockResponse = new SBApiResponse(Constants.USER_NOTIFICATION_READ_N_DAYSID);
        mockResponse.setParams(new SunbirdApiRespParam("read-success"));

        Instant recentInstant = ZonedDateTime.now(ZoneOffset.UTC).minusDays(1).toInstant();

        Map<String, Object> notif1 = new HashMap<>();
        notif1.put(Constants.NOTIFICATION_ID, "notif1");
        notif1.put(Constants.CREATED_AT, recentInstant);
        notif1.put(Constants.READ, false);
        notif1.put(Constants.TYPE, "INFO");
        notif1.put(Constants.MESSAGE, "Unread Message");
        notif1.put(Constants.ROLE, "user");
        notif1.put(Constants.SOURCE, "system");
        notif1.put(Constants.CATEGORY, "general");

        Map<String, Object> notif2 = new HashMap<>(notif1);
        notif2.put(Constants.NOTIFICATION_ID, "notif2");
        notif2.put(Constants.READ, true);
        notif2.put(Constants.MESSAGE, "Read Message");

        List<Map<String, Object>> mockRecords = List.of(notif1, notif2);

        when(transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_READ_N_DAYSID)).thenReturn(mockResponse);
        when(accessTokenValidator.verifyUserToken(token)).thenReturn(userId);
        when(cassandraOperation.getRecordsByPropertiesWithoutFiltering(
                eq(Constants.KEYSPACE_SUNBIRD),
                eq(Constants.TABLE_USER_NOTIFICATION),
                anyMap(),
                anyList(),
                anyInt()
        )).thenReturn(mockRecords);


        SBApiResponse response = notificationService.readByUserIdAndLastXDaysNotifications(token, days, page, size, NotificationReadStatus.BOTH);

        assertEquals(HttpStatus.OK, response.getResponseCode());
        Map<String, Object> result = response.getResult();
        assertNotNull(result);
        assertTrue(result.containsKey(Constants.NOTIFICATIONS));
        assertEquals(2, ((List<?>) result.get(Constants.NOTIFICATIONS)).size());
        assertEquals(2, result.get(Constants.TOTAL));
        assertEquals(0, result.get(Constants.PAGE));
        assertEquals(2, result.get(Constants.SIZE));
        assertEquals(false, result.get(Constants.HAS_NEXT_PAGE));
    }

    @Test
    void testMarkNotificationsAsRead_Success() {

        String token = "valid_token";
        String userId = "user123";
        String notificationId = "notif-001";
        Instant createdAt = Instant.now();

        SBApiResponse mockResponse = new SBApiResponse(Constants.USER_NOTIFICATION_READ_UPDATEID);
        mockResponse.setParams(new SunbirdApiRespParam("update-id"));

        Map<String, Object> notificationRecord = new HashMap<>();
        notificationRecord.put(Constants.NOTIFICATION_ID, notificationId);
        notificationRecord.put(Constants.USER_ID, userId);
        notificationRecord.put(Constants.CREATED_AT, createdAt);
        notificationRecord.put(Constants.READ, false);

        Map<String, Object> updateResult = Map.of(Constants.RESPONSE, Constants.SUCCESS);

        when(transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_READ_UPDATEID)).thenReturn(mockResponse);
        when(accessTokenValidator.verifyUserToken(token)).thenReturn(userId);
        when(cassandraOperation.getRecordsByPropertiesWithoutFiltering(
                eq(Constants.KEYSPACE_SUNBIRD),
                eq(Constants.TABLE_USER_NOTIFICATION),
                anyMap(),
                isNull(),
                anyInt()
        )).thenReturn(List.of(notificationRecord));
        when(cassandraOperation.updateRecord(
                anyString(), anyString(), anyMap(), anyMap())
        ).thenReturn(updateResult);


        SBApiResponse response = notificationService.markNotificationsAsRead(token, List.of(notificationId));


        assertEquals(HttpStatus.OK, response.getResponseCode());
        assertEquals(Constants.SUCCESS, response.getParams().getStatus());

        Map<String, Object> result = response.getResult();
        assertTrue(result.containsKey("notifications"));

        List<?> updatedList = (List<?>) result.get("notifications");
        assertEquals(1, updatedList.size());

        Map<?, ?> updatedItem = (Map<?, ?>) updatedList.get(0);
        assertEquals(notificationId, updatedItem.get(Constants.ID));
        assertEquals(true, updatedItem.get(Constants.READ));
        assertNotNull(updatedItem.get(Constants.READ_AT));
    }

    @Test
    void testMarkNotificationsAsDeleted_Success() {

        String token = "valid_token";
        String userId = "user123";
        String notificationId = "notif-delete-001";
        Instant createdAt = Instant.now();

        SBApiResponse mockResponse = new SBApiResponse(Constants.USER_NOTIFICATION_DELETE);
        mockResponse.setParams(new SunbirdApiRespParam("delete-id"));

        Map<String, Object> notification = new HashMap<>();
        notification.put(Constants.NOTIFICATION_ID, notificationId);
        notification.put(Constants.USER_ID, userId);
        notification.put(Constants.CREATED_AT, createdAt);
        notification.put(Constants.IS_DELETED, false);

        Map<String, Object> updateResult = Map.of(Constants.RESPONSE, Constants.SUCCESS);

        when(transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_DELETE)).thenReturn(mockResponse);
        when(accessTokenValidator.verifyUserToken(token)).thenReturn(userId);
        when(cassandraOperation.getRecordsByPropertiesWithoutFiltering(
                eq(Constants.KEYSPACE_SUNBIRD),
                eq(Constants.TABLE_USER_NOTIFICATION),
                anyMap(),
                isNull(),
                anyInt()
        )).thenReturn(List.of(notification));
        when(cassandraOperation.updateRecord(
                anyString(), anyString(), anyMap(), anyMap())
        ).thenReturn(updateResult);


        SBApiResponse response = notificationService.markNotificationsAsDeleted(token, List.of(notificationId));


        assertEquals(HttpStatus.OK, response.getResponseCode());
        assertEquals(Constants.SUCCESS, response.getParams().getStatus());

        Map<String, Object> result = response.getResult();
        assertTrue(result.containsKey(Constants.NOTIFICATIONS));

        List<?> deletedList = (List<?>) result.get(Constants.NOTIFICATIONS);
        assertEquals(1, deletedList.size());

        Map<?, ?> deletedItem = (Map<?, ?>) deletedList.get(0);
        assertEquals(notificationId, deletedItem.get(Constants.ID));
        assertEquals(true, deletedItem.get(Constants.IS_DELETED));
    }

    @Test
    void testMarkNotificationsAsDeleted_AlreadyDeleted() {
        String token = "valid_token";
        String userId = "user123";
        String notificationId = "notif-delete-002";
        Instant createdAt = Instant.now();

        SBApiResponse mockResponse = new SBApiResponse(Constants.USER_NOTIFICATION_DELETE);
        mockResponse.setParams(new SunbirdApiRespParam("delete-id"));

        Map<String, Object> notification = new HashMap<>();
        notification.put(Constants.NOTIFICATION_ID, notificationId);
        notification.put(Constants.USER_ID, userId);
        notification.put(Constants.CREATED_AT, createdAt);
        notification.put(Constants.IS_DELETED, true);

        when(transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_DELETE)).thenReturn(mockResponse);
        when(accessTokenValidator.verifyUserToken(token)).thenReturn(userId);
        when(cassandraOperation.getRecordsByPropertiesWithoutFiltering(any(), any(), any(), any(), anyInt()))
                .thenReturn(List.of(notification));

        SBApiResponse response = notificationService.markNotificationsAsDeleted(token, List.of(notificationId));

        assertEquals(HttpStatus.OK, response.getResponseCode());
        assertEquals(Constants.SUCCESS, response.getParams().getStatus());

        List<?> notifications = (List<?>) response.getResult().get(Constants.NOTIFICATIONS);
        assertEquals(0, notifications.size(), "Should not return any updated items if already deleted");
    }

    @Test
    void testMarkNotificationsAsDeleted_MissingCreatedAt() {
        String token = "valid_token";
        String userId = "user123";
        String notificationId = "notif-missing-createdAt";

        SBApiResponse mockResponse = new SBApiResponse(Constants.USER_NOTIFICATION_DELETE);
        mockResponse.setParams(new SunbirdApiRespParam("delete-id"));

        Map<String, Object> notification = new HashMap<>();
        notification.put(Constants.NOTIFICATION_ID, notificationId);
        notification.put(Constants.USER_ID, userId);
        notification.put(Constants.IS_DELETED, false); // not deleted
        notification.put(Constants.CREATED_AT, null); // missing

        when(transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_DELETE)).thenReturn(mockResponse);
        when(accessTokenValidator.verifyUserToken(token)).thenReturn(userId);
        when(cassandraOperation.getRecordsByPropertiesWithoutFiltering(any(), any(), any(), any(), anyInt()))
                .thenReturn(List.of(notification));

        SBApiResponse response = notificationService.markNotificationsAsDeleted(token, List.of(notificationId));

        assertEquals(HttpStatus.OK, response.getResponseCode());
        List<?> resultList = (List<?>) response.getResult().get(Constants.NOTIFICATIONS);
        assertTrue(resultList.isEmpty(), "Should skip when createdAt is missing");
    }

    @Test
    void testMarkNotificationsAsDeleted_ExceptionThrown() {
        String token = "valid_token";
        String userId = "user123";
        String notificationId = "notif-error";

        when(accessTokenValidator.verifyUserToken(token)).thenReturn(userId);
        when(cassandraOperation.getRecordsByPropertiesWithoutFiltering(any(), any(), any(), any(), anyInt()))
                .thenThrow(new RuntimeException("Simulated DB failure"));

        assertThrows(CustomException.class, () -> {
            notificationService.markNotificationsAsDeleted(token, List.of(notificationId));
        });
    }

    @Test
    void testCreateNotification_MissingRequestNode() {
        String token = "valid_token";
        String userId = "user123";

        ObjectNode invalidPayload = objectMapper.createObjectNode(); // no "request"

        SBApiResponse mockResponse = new SBApiResponse(Constants.USER_NOTIFICATION_CREATE);
        mockResponse.setParams(new SunbirdApiRespParam("msg123"));

        when(accessTokenValidator.verifyUserToken(token)).thenReturn(userId);
        when(transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_CREATE)).thenReturn(mockResponse);

        SBApiResponse response = notificationService.createNotification(invalidPayload, token);

        assertEquals(HttpStatus.BAD_REQUEST, response.getResponseCode());
        assertEquals(Constants.FAILED, response.getParams().getStatus());
        assertEquals("Missing or invalid 'request' node in payload", response.getParams().getMsg());
    }

    @Test
    void testCreateNotification_InvalidToken() {
        String token = "invalid_token";

        ObjectNode requestPayload = objectMapper.createObjectNode();
        requestPayload.put("type", "INFO");
        requestPayload.put("message", "Hello!");

        ObjectNode mainPayload = objectMapper.createObjectNode();
        mainPayload.set("request", requestPayload);

        when(accessTokenValidator.verifyUserToken(token)).thenReturn(Constants.UNAUTHORIZED);
        SBApiResponse mockResponse = new SBApiResponse(Constants.USER_NOTIFICATION_CREATE);
        mockResponse.setParams(new SunbirdApiRespParam());

        when(transformUtility.createDefaultResponse(Constants.USER_NOTIFICATION_CREATE)).thenReturn(mockResponse);

        SBApiResponse response = notificationService.createNotification(mainPayload, token);

        assertEquals(HttpStatus.BAD_REQUEST, response.getResponseCode());
        assertEquals(Constants.FAILED, response.getParams().getStatus());
        assertEquals(Constants.USER_ID_DOESNT_EXIST, response.getParams().getMsg());
    }

}


