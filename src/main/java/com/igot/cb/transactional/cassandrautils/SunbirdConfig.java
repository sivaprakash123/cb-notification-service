package com.igot.cb.transactional.cassandrautils;

import com.datastax.oss.driver.api.core.CqlSession;
import com.datastax.oss.driver.api.core.CqlSessionBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.cassandra.core.CassandraAdminTemplate;
import org.springframework.data.cassandra.repository.config.EnableCassandraRepositories;

import javax.validation.constraints.NotNull;
import java.net.InetSocketAddress;
import java.util.Objects;

@Configuration
@ConfigurationProperties("spring.cassandra")
@EnableCassandraRepositories(basePackages = {"org.sunbird"}, cassandraTemplateRef = "sunbirdTemplate")
public class SunbirdConfig extends CassandraConfig {
    private Logger logger = LoggerFactory.getLogger(SunbirdConfig.class);
    @Value("${spring.cassandra.username}")
    private String sunbirdUser;
    @Value("${spring.cassandra.password}")
    private String sunbirdPassword;

    @NotNull
    @Bean(name = "sunbirdTemplate")
    public CassandraAdminTemplate cassandraTemplate(@Autowired CqlSession cqlSession) {
        logger.info("Creating CassandraAdminTemplate for keyspace: {}", getKeyspaceName());
        return new CassandraAdminTemplate(cqlSession, cassandraConverter());
    }

    @Primary
    @Bean(name = "sunbirdSession")
    public CqlSession cqlSession() {
        logger.info("Creating CqlSession for keyspace: {}", getKeyspaceName());
        CqlSessionBuilder builder = CqlSession.builder();
        String[] contactPoints = getContactPoints().split(",");

        for (String contactPoint : contactPoints) {
            builder.addContactPoint(new InetSocketAddress(contactPoint.trim(), getPort()));
        }
        builder.withLocalDatacenter(Objects.requireNonNull(getLocalDataCenter()))
                .withKeyspace(getKeyspaceName());
        if (!sunbirdUser.isEmpty() && !sunbirdPassword.isEmpty()) {
            builder.withAuthCredentials(sunbirdUser, sunbirdPassword);
        }
        return builder.build();
    }
}
