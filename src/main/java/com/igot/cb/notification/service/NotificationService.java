package com.igot.cb.notification.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.igot.cb.notification.enums.NotificationReadStatus;
import com.igot.cb.util.ApiResponse;


import java.util.List;

public interface NotificationService {

    ApiResponse createNotification(JsonNode userNotificationDetail, String token);

    ApiResponse readByUserIdAndNotificationId(String notificationId, String token);

    ApiResponse readByUserIdAndLastXDaysNotifications(String token, int days, int page, int size, NotificationReadStatus status);

    ApiResponse markNotificationsAsRead(String token, List<String> notificationIds);

    ApiResponse markNotificationsAsDeleted(String token, List<String> notificationIds);

}
