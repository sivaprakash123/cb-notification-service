package com.igot.cb.notification.controller;

import com.fasterxml.jackson.databind.JsonNode;
import com.igot.cb.notification.service.NotificationService;

import com.igot.cb.util.Constants;
import com.igot.cb.util.dto.SBApiResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/notifications")
public class NotificationController {

    @Autowired
    NotificationService notificationService;

    @PostMapping("/create")
    public ResponseEntity<SBApiResponse> createNotification(@RequestBody JsonNode discussionDetails,
                                                        @RequestHeader(Constants.X_AUTH_TOKEN) String token) {
        SBApiResponse response = notificationService.createNotification(discussionDetails,token);
        return new ResponseEntity<>(response, response.getResponseCode());
    }

    @GetMapping("/readby/{notificationId}")
    public ResponseEntity<?> readByUserIdAndCourseId(@PathVariable String notificationId, @RequestHeader(Constants.X_AUTH_TOKEN) String token) {
        SBApiResponse response = notificationService.readByUserIdAndNotificationId(notificationId,token);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @GetMapping("/list")
    public ResponseEntity<?> getLast30DaysNotifications(
            @RequestHeader(Constants.X_AUTH_TOKEN) String token,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size) {
        SBApiResponse response = notificationService.readByUserIdAndLast30DaysNotifications(token, page, size);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @PatchMapping("/read")
    public ResponseEntity<?> markNotificationsAsRead(
            @RequestHeader(Constants.X_AUTH_TOKEN) String token,
            @RequestBody Map<String, Object> requestBody) {

        List<String> ids = (List<String>) ((Map<String, Object>) requestBody.get("request")).get("ids");
        SBApiResponse response = notificationService.markNotificationsAsRead(token, ids);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }


}
