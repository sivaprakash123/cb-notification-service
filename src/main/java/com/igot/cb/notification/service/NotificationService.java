package com.igot.cb.notification.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.igot.cb.notification.enums.NotificationReadStatus;
import com.igot.cb.util.dto.SBApiResponse;

import java.util.List;

public interface NotificationService {

    SBApiResponse createNotification(JsonNode userNotificationDetail, String token);

    SBApiResponse readByUserIdAndNotificationId(String notificationId, String token);

    SBApiResponse readByUserIdAndLastXDaysNotifications(String token, int days, int page, int size, NotificationReadStatus status);

    SBApiResponse markNotificationsAsRead(String token, List<String> notificationIds);

    SBApiResponse markNotificationsAsDeleted(String token, List<String> notificationIds);

}
