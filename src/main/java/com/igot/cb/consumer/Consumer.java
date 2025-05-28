package com.igot.cb.consumer;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.igot.cb.notification.service.NotificationService;
import com.igot.cb.util.Constants;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;


@Service
public class Consumer {

    private static final Logger logger = LogManager.getLogger(Consumer.class);

    @Autowired
    private NotificationService notificationService;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @KafkaListener(topics = "${kafka.topic.name}", groupId = "${kafka.consumer.group-id}")

    public void consume(String message) {
        logger.info("Received message from Kafka: {}", message);

        try {
            JsonNode jsonNode = objectMapper.readTree(message);

            if (!jsonNode.has(Constants.REQUEST)) {
                logger.warn("Missing 'request' field in message: {}", message);
                return;
            }

            notificationService.bulkCreateNotifications(jsonNode);

            logger.info("Notification successfully forwarded to service");

        } catch (Exception e) {
            logger.error("Failed to process Kafka message", e);
        }
    }
}
