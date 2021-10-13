package ojt.management.data.rsql;

import cz.jirutka.rsql.parser.ast.ComparisonOperator;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.util.List;
import java.util.stream.Collectors;

public class GenericRsqlSpecification<T> implements Specification<T> {

    private String property;
    private ComparisonOperator operator;
    private List<String> arguments;

    public GenericRsqlSpecification(final String property, final ComparisonOperator operator, final List<String> arguments) {
        super();
        this.property = property;
        this.operator = operator;
        this.arguments = arguments;
    }

    @Override
    public Predicate toPredicate(final Root<T> root, final CriteriaQuery<?> query, final CriteriaBuilder builder) {
        Path<String> propertyExpression = parseProperty(root);
        List<Object> args = castArguments(propertyExpression);
        final Object argument = args.get(0);
        switch (RsqlSearchOperation.getSimpleOperator(operator)) {

        case EQUAL: {
            if (argument instanceof String) {
                return builder.like(root.get(property), argument.toString().replace('*', '%'));
            } else if (argument == null) {
                return builder.isNull(root.get(property));
            } else {
                return builder.equal(root.get(property), argument);
            }
        }
        case NOT_EQUAL: {
            if (argument instanceof String) {
                return builder.notLike(root.<String> get(property), argument.toString().replace('*', '%'));
            } else if (argument == null) {
                return builder.isNotNull(root.get(property));
            } else {
                return builder.notEqual(root.get(property), argument);
            }
        }
        case GREATER_THAN: {
            return builder.greaterThan(root.<String> get(property), argument.toString());
        }
        case GREATER_THAN_OR_EQUAL: {
            return builder.greaterThanOrEqualTo(root.<String> get(property), argument.toString());
        }
        case LESS_THAN: {
            return builder.lessThan(root.<String> get(property), argument.toString());
        }
        case LESS_THAN_OR_EQUAL: {
            return builder.lessThanOrEqualTo(root.<String> get(property), argument.toString());
        }
        case IN:
            return root.get(property).in(args);
        case NOT_IN:
            return builder.not(root.get(property).in(args));
        }

        return null;
    }

    // === private

    // This method will help us diving deep into nested property using the dot convention
    // The originial tutorial did not have this, so it can only parse the shallow properties.
    private Path<String> parseProperty(Root<T> root) {
        Path<String> path;
        if (property.contains(".")) {
            // Nested properties
            String[] pathSteps = property.split("\\.");
            String step = pathSteps[0];
            path = root.get(step);

            for (int i = 1; i <= pathSteps.length - 1; i++) {
                path = path.get(pathSteps[i]);
            }
        } else {
            path = root.get(property);
        }
        return path;
    }

    private List<Object> castArguments(Path<?> propertyExpression) {
        Class<?> type = propertyExpression.getJavaType();

        return arguments.stream().map(arg -> {
            if (type.equals(Integer.class)) return Integer.parseInt(arg);
            else if (type.equals(Long.class)) return Long.parseLong(arg);
            else if (type.equals(Byte.class)) return Byte.parseByte(arg);
            else return arg;
        }).collect(Collectors.toList());
    }

}
