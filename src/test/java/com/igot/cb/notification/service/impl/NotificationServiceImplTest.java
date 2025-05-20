package com.igot.cb.notification.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.igot.cb.authentication.util.AccessTokenValidator;
import com.igot.cb.transactional.cassandrautils.CassandraOperation;
import com.igot.cb.util.ApiResponse;
import com.igot.cb.util.Constants;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.*;
import org.springframework.http.HttpStatus;


import java.time.Instant;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

class NotificationServiceImplTest {

    @InjectMocks
    private NotificationServiceImpl notificationService;

    @Mock
    private AccessTokenValidator accessTokenValidator;

    @Mock
    private CassandraOperation cassandraOperation;

    @Mock
    private ObjectMapper objectMapper;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testCreateNotification_Success_WithComplexMessage() throws Exception {
        // Prepare input
        String authToken = "Bearer token";
        String userId = "testUser";
        String notificationType = "comment";
        String category = "content";
        String source = "userCreated";
        String role = "user";


        String payload = "{ \"request\": { " +
                "\"type\": \"" + notificationType + "\"," +
                "\"category\": \"" + category + "\"," +
                "\"source\": \"" + source + "\"," +
                "\"role\": \"" + role + "\"," +
                "\"message\": { " +
                "\"topic\": \"subscriber-updates\"," +
                "\"notification\": { \"body\": \"This week's edition is now available.\", \"title\": \"NewsMagazine.com\" }," +
                "\"data\": { \"volume\": \"3.21.15\", \"contents\": \"http://www.news-magazine.com/world-week/21659772\" }" +
                "}" +
                "} }";

        ObjectMapper mapper = new ObjectMapper();
        JsonNode userNotificationDetail = mapper.readTree(payload);

        when(accessTokenValidator.fetchUserIdFromAccessToken(authToken)).thenReturn(userId);
        when(cassandraOperation.insertRecord(anyString(), anyString(), anyMap()))
                .thenReturn(Map.of("response", "SUCCESS"));

        doAnswer(invocation -> {
            String msg = invocation.getArgument(0, String.class);
            return mapper.readTree(msg);
        }).when(objectMapper).readTree(anyString());

        ApiResponse response = notificationService.createNotification(userNotificationDetail, authToken);

        assertEquals(HttpStatus.OK, response.getResponseCode());
        assertNotNull(response.getResult());
        Map<String, Object> result = (Map<String, Object>) response.getResult();

        assertFalse(result.containsKey(Constants.IS_DELETED));
        assertFalse(result.containsKey(Constants.UPDATED_AT));
        assertFalse(result.containsKey(Constants.USER_ID));
        assertFalse(result.containsKey(Constants.READ_AT));
        assertFalse(result.containsKey(Constants.TEMPLATE_ID));
        assertEquals(notificationType, result.get(Constants.TYPE));
        assertEquals(role, result.get(Constants.ROLE));
        assertEquals(source, result.get(Constants.SOURCE));
        assertEquals(category, result.get(Constants.CATEGORY));

        Object messageField = result.get(Constants.MESSAGE);
        assertNotNull(messageField, "MESSAGE field should not be null");
        assertTrue(messageField instanceof JsonNode);
        JsonNode messageNode = (JsonNode) messageField;
        assertEquals("subscriber-updates", messageNode.get("topic").asText());
        assertEquals("NewsMagazine.com", messageNode.get("notification").get("title").asText());
        assertEquals("3.21.15", messageNode.get("data").get("volume").asText());
    }

    @Test
    void testCreateNotification_MissingUserId() throws Exception {
        String authToken = "Bearer token";
        ObjectMapper mapper = new ObjectMapper();
        String payload = "{ \"request\": { \"type\": \"comment\" } }";
        JsonNode userNotificationDetail = mapper.readTree(payload);

        when(accessTokenValidator.fetchUserIdFromAccessToken(authToken)).thenReturn("");

        ApiResponse response = notificationService.createNotification(userNotificationDetail, authToken);

        assertEquals(HttpStatus.BAD_REQUEST, response.getResponseCode());
        assertEquals(Constants.FAILED, response.getParams().getStatus());
        assertEquals(Constants.USER_ID_DOESNT_EXIST, response.getParams().getErrMsg());
    }

    @Test
    void testCreateNotification_MissingRequestNode() throws Exception {
        String authToken = "Bearer token";
        String userId = "testUser";
        ObjectMapper mapper = new ObjectMapper();
        String payload = "{}";
        JsonNode userNotificationDetail = mapper.readTree(payload);

        when(accessTokenValidator.fetchUserIdFromAccessToken(authToken)).thenReturn(userId);

        ApiResponse response = notificationService.createNotification(userNotificationDetail, authToken);

        assertEquals(HttpStatus.BAD_REQUEST, response.getResponseCode());
        assertEquals(Constants.FAILED, response.getParams().getStatus());
        assertEquals("Missing or invalid 'request' node in payload", response.getParams().getErrMsg());
    }

    @Test
    void testCreateNotification_MessageStringParsingFails() throws Exception {
        // This test simulates when the message field is a string but not valid JSON
        String authToken = "Bearer token";
        String userId = "testUser";
        String notificationType = "comment";
        String category = "content";
        String source = "userCreated";
        String role = "user";
        // message is a string, not a JSON object
        String payload = "{ \"request\": { " +
                "\"type\": \"" + notificationType + "\"," +
                "\"category\": \"" + category + "\"," +
                "\"source\": \"" + source + "\"," +
                "\"role\": \"" + role + "\"," +
                "\"message\": \"This is not JSON\"" +
                "} }";
        ObjectMapper mapper = new ObjectMapper();
        JsonNode userNotificationDetail = mapper.readTree(payload);

        when(accessTokenValidator.fetchUserIdFromAccessToken(authToken)).thenReturn(userId);
        when(cassandraOperation.insertRecord(anyString(), anyString(), anyMap()))
                .thenReturn(Map.of("response", "SUCCESS"));
        doThrow(new RuntimeException("Not a JSON")).when(objectMapper).readTree("This is not JSON");

        ApiResponse response = notificationService.createNotification(userNotificationDetail, authToken);

        assertEquals(HttpStatus.OK, response.getResponseCode());
        assertNotNull(response.getResult());
        Map<String, Object> result = (Map<String, Object>) response.getResult();
        assertEquals("This is not JSON", result.get(Constants.MESSAGE));
    }

    @Test
    void testMarkNotificationsAsRead_EmptyList() {
        String authToken = "Bearer abc";
        String userId = "user-1";
        when(accessTokenValidator.fetchUserIdFromAccessToken(authToken)).thenReturn(userId);

        ApiResponse response = notificationService.markNotificationsAsRead(authToken, Collections.emptyList());
        assertEquals(HttpStatus.BAD_REQUEST, response.getResponseCode());
        assertEquals(Constants.FAILED, response.getParams().getStatus());
        assertTrue(response.getParams().getErrMsg().contains("non-empty list"));
    }
    @Test
    void testMarkNotificationsAsDeleted_TooManyIds() {
        String authToken = "Bearer abc";
        String userId = "user-1";
        int batchSize = Constants.MAX_NOTIFICATION_READ_BATCH_SIZE + 1;
        List<String> notificationIds = new ArrayList<>();
        for (int i = 0; i < batchSize; i++) notificationIds.add("id-" + i);

        when(accessTokenValidator.fetchUserIdFromAccessToken(authToken)).thenReturn(userId);

        ApiResponse response = notificationService.markNotificationsAsDeleted(authToken, notificationIds);
        assertEquals(HttpStatus.BAD_REQUEST, response.getResponseCode());
        assertEquals(Constants.FAILED, response.getParams().getStatus());
        assertTrue(response.getParams().getErrMsg().contains("only mark up to"));
    }

    @Test
    void testPrepareNotificationResponse_MessageIsNonString() {
        Map<String, Object> dbRecord = new HashMap<>();
        dbRecord.put("message", 12345); // not a String

        // objectMapper.readTree should not be called
        Map<String, Object> result = notificationService.prepareNotificationResponse(dbRecord);
        assertEquals(12345, result.get("message"));
    }

    @Test
    void testPrepareNotificationResponse_MessageIsNull() {
        Map<String, Object> dbRecord = new HashMap<>();
        dbRecord.put("message", null);

        Map<String, Object> result = notificationService.prepareNotificationResponse(dbRecord);
        assertNull(result.get("message"));
    }

}