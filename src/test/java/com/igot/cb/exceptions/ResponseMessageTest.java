package com.igot.cb.exceptions;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ResponseMessageTest {

    @Test
    void testMessageConstants() {
        assertEquals("You are not authorized.", ResponseMessage.Message.UNAUTHORIZED_USER);
        assertEquals("Process failed,please try again later.", ResponseMessage.Message.INTERNAL_ERROR);
    }

    @Test
    void testKeyConstants() {
        assertEquals("UNAUTHORIZED_USER", ResponseMessage.Key.UNAUTHORIZED_USER);
        assertEquals("INTERNAL_ERROR", ResponseMessage.Key.INTERNAL_ERROR);
    }
}