package com.igot.cb.notification.controller;

import com.fasterxml.jackson.databind.JsonNode;
import com.igot.cb.notification.enums.NotificationReadStatus;
import com.igot.cb.notification.service.NotificationService;
import com.igot.cb.util.ApiResponse;
import com.igot.cb.util.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

import static com.igot.cb.util.Constants.IDS;
import static com.igot.cb.util.Constants.REQUEST;

@RestController
@RequestMapping("/v1/notifications")
public class NotificationController {


    @Autowired
    NotificationService notificationService;

    @PostMapping("/create")
    public ResponseEntity<ApiResponse> createNotification(@RequestBody JsonNode userNotificationDetail,
                                                          @RequestHeader(Constants.X_AUTH_TOKEN) String token) {
        ApiResponse response = notificationService.createNotification(userNotificationDetail, token);
        return new ResponseEntity<>(response, response.getResponseCode());
    }

    @GetMapping("/readby/{notificationId}")
    public ResponseEntity<?> readByUserIdAndNotificationId(@PathVariable String notificationId, @RequestHeader(Constants.X_AUTH_TOKEN) String token) {
        ApiResponse response = notificationService.readByUserIdAndNotificationId(notificationId, token);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }


    @GetMapping("/list")
    public ResponseEntity<?> getLastXDaysNotifications(
            @RequestHeader(Constants.X_AUTH_TOKEN) String token,
            @RequestParam(defaultValue = Constants.DEFAULT_NOTIFICATION_DAYS + "") int days,
            @RequestParam(defaultValue = Constants.DEFAULT_NOTIFICATION_PAGE + "") int page,
            @RequestParam(defaultValue = Constants.DEFAULT_NOTIFICATION_PAGE_SIZE + "") int size,
            @RequestParam(defaultValue = Constants.DEFAULT_NOTIFICATION_READ_STATUS) NotificationReadStatus status) {

        ApiResponse response = notificationService.readByUserIdAndLastXDaysNotifications(token, days, page, size, status);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }


    @PatchMapping("/read")
    public ResponseEntity<?> markNotificationsAsRead(
            @RequestHeader(Constants.X_AUTH_TOKEN) String token,
            @RequestBody Map<String, Object> requestBody) {

        List<String> ids = (List<String>) ((Map<String, Object>) requestBody.get(REQUEST)).get(IDS);
        ApiResponse response = notificationService.markNotificationsAsRead(token, ids);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @DeleteMapping("/delete")
    public ResponseEntity<?> markNotificationsAsDeleted(
            @RequestHeader(Constants.X_AUTH_TOKEN) String token,
            @RequestBody Map<String, Object> requestBody) {

        List<String> ids = (List<String>) ((Map<String, Object>) requestBody.get(REQUEST)).get(IDS);
        ApiResponse response = notificationService.markNotificationsAsDeleted(token, ids);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }
}
