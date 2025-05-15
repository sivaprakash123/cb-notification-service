package com.igot.cb.util;


public class Constants {
    public static final String KEYSPACE_SUNBIRD = "sunbird";
    public static final String KEYSPACE_SUNBIRD_COURSES = "sunbird_courses";
    public static final String CORE_CONNECTIONS_PER_HOST_FOR_LOCAL = "coreConnectionsPerHostForLocal";
    public static final String CORE_CONNECTIONS_PER_HOST_FOR_REMOTE = "coreConnectionsPerHostForRemote";
    public static final String MAX_CONNECTIONS_PER_HOST_FOR_LOCAL = "maxConnectionsPerHostForLocal";
    public static final String MAX_CONNECTIONS_PER_HOST_FOR_REMOTE = "maxConnectionsPerHostForRemote";
    public static final String MAX_REQUEST_PER_CONNECTION = "maxRequestsPerConnection";
    public static final String HEARTBEAT_INTERVAL = "heartbeatIntervalSeconds";
    public static final String POOL_TIMEOUT = "poolTimeoutMillis";
    public static final String CASSANDRA_CONFIG_HOST = "cassandra.config.host";
    public static final String SUNBIRD_CASSANDRA_CONSISTENCY_LEVEL = "consistencyLevel";
    public static final String EXCEPTION_MSG_FETCH = "Exception occurred while fetching record from ";
    public static final String INSERT_INTO = "INSERT INTO ";
    public static final String DOT = ".";
    public static final String OPEN_BRACE = "(";
    public static final String VALUES_WITH_BRACE = ") VALUES (";
    public static final String QUE_MARK = "?";
    public static final String COMMA = ",";
    public static final String CLOSING_BRACE = ");";
    public static final String RESPONSE = "response";
    public static final String SUCCESS = "success";
    public static final String FAILED = "Failed";
    public static final String ERROR_MESSAGE = "errmsg";
    public static final String ERROR = "ERROR";
    public static final String REDIS_KEY_PREFIX = "cbextenroll_";
    public static final String CBPORES_REDIS_KEY_PREFIX = "cbpores_";
    public static final String DOT_SEPARATOR = ".";
    public static final String SHA_256_WITH_RSA = "SHA256withRSA";
    public static final String UNAUTHORIZED = "Unauthorized";
    public static final String SUB = "sub";
    public static final String SSO_URL = "sso.url";
    public static final String SSO_REALM = "sso.realm";
    public static final String ACCESS_TOKEN_PUBLICKEY_BASEPATH = "accesstoken.publickey.basepath";
    public static final String API_VERSION_1 = "1.0";
    public static final String X_AUTH_TOKEN = "x-authenticated-user-token";
    public static final String USER_ID_DOESNT_EXIST = "User Id doesn't exist! Please supply a valid auth token";

    public static final String LIMIT = "limit";
    public static final String LOCAL_DATACENTER = "spring.cassandra.local-datacenter";

    public static final String USER_NOTIFICATION_CREATE="notification.v1.create";
    public static final String TABLE_USER_NOTIFICATION = "notifications";
    public static final String USER_NOTIFICATION_READ_NOTIFICATIONID="notification.v1.readby.useridnotificationid";
    public static final String USER_NOTIFICATION_READ_N_DAYSID="notification.v1.readby.ndays";
    public static final String USER_NOTIFICATION_READ_UPDATEID="notification.v1.update.notificationid";
    public static final  String USER_NOTIFICATION_DELETE ="notification.v1.delete.notificationid";
    public static final Integer MAX_NOTIFICATION_READ_BATCH_SIZE =20;


    public static final String NOTIFICATION_ID = "notification_id";
    public static final String USER_ID = "user_id";
    public static final String CREATED_AT = "created_at";
    public static final String UPDATED_AT = "updated_at";
    public static final String IS_DELETED = "is_deleted";
    public static final String READ = "read";
    public static final String READ_AT = "read_at";
    public static final String UTC = "UTC";
    public static final String TEMPLATE_ID = "template_id";
    public static final String TYPE = "type";
    public static final String MESSAGE = "message";
    public static final String ROLE = "role";
    public static final String SOURCE = "source";
    public static final String CATEGORY = "category";
    public static final String NOTIFICATIONS = "notifications";
    public static final String TOTAL = "total";
    public static final String PAGE = "page";
    public static final String SIZE = "size";
    public static final String HAS_NEXT_PAGE = "hasNextPage";
    public static final String ID = "id";
    public static final String REQUEST = "request";
    public static final String IDS = "ids";
    public static final int MAX_NOTIFICATIONS_FETCH_FOR_READ = 100;

    public static final int HTTP_CLIENT_TIMEOUT_MS = 45000;
    public static final int HTTP_CLIENT_MAX_TOTAL_CONNECTIONS = 2000;
    public static final int HTTP_CLIENT_MAX_CONNECTIONS_PER_ROUTE = 500;

    // Redis
    public static final String API_REDIS_DELETE = "api.redis.delete";
    public static final String API_REDIS_GET_KEYS = "api.redis.get.keys";
    public static final String API_REDIS_GET_KEYS_VALUE_SET = "api.redis.get.keys&values";
    public static final String REDIS_COMMON_KEY = "CB_EXT_";
    public static final String COMPETENCY_CACHE_NAME = "competency";
    public static final String COMPETENCY_CACHE_NAME_BY_AREA = "competencyByArea";
    public static final String COMPETENCY_CACHE_NAME_BY_TYPE = "competencyByType";
    public static final String SUCCESSFUL = "Successful";
    public static final String QUESTION_ID = "qs_id_";


    public static final String API_HEALTH_CHECK = "api.health.check";
    public static final String DRAFT = "DRAFT";
    public static final Object CREATED = "Created";
    public static final Object UPDATED = "Updated";
    public static final String HEALTHY = "healthy";
    public static final String CHECKS = "checks";
    public static final String CASSANDRA_DB = "cassandra db";
    public static final String REDIS_CACHE = "redis cache";
    public static final String NAME = "name";
    public static final String TABLE_SYSTEM_SETTINGS = "system_settings";

    private Constants() {
    }
}
