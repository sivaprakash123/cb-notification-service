package com.igot.cb.health.service;


import com.igot.cb.util.dto.SBApiResponse;

public interface HealthService {

    SBApiResponse checkHealthStatus() throws Exception;

}
