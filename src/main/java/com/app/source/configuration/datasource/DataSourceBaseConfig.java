package com.app.source.configuration.datasource;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.cfg.Environment;
import org.springframework.boot.orm.jpa.EntityManagerFactoryBuilder;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;

import javax.sql.DataSource;
import java.util.HashMap;
import java.util.Map;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class DataSourceBaseConfig {
    private String dialect;
    private String dllMethod;
    private String driver;
    private String showSql;
    private String basePackage;
    private String sourceUnit;


    private Map<String, String> getProperties() {
        final Map<String, String> properties = new HashMap<>();
        properties.put(Environment.HBM2DDL_AUTO, getDllMethod());
        properties.put(Environment.DIALECT, getDialect());
        properties.put(Environment.DRIVER, getDriver());
        properties.put(Environment.SHOW_SQL, getShowSql());
        return properties;
    }

    protected LocalContainerEntityManagerFactoryBean
    buildEntityManagerFactory(EntityManagerFactoryBuilder builder,
                              DataSource dataSource) {
        final Map<String, String> properties = getProperties();
        return builder
                .dataSource(dataSource)
                .properties(properties)
                .packages(getBasePackage())
                .persistenceUnit(getSourceUnit())
                .build();
    }


}
