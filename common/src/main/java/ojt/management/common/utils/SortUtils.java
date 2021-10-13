package ojt.management.common.utils;

import org.springframework.data.domain.Sort;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SortUtils {
    public static Sort parseSortQuery(String query) {
        Pattern pattern = Pattern.compile("(\\w+?)( )(ASC|DESC|>|<|asc|desc),");
        Matcher matcher = pattern.matcher(query + ",");
        Sort sort = null;
        while (matcher.find()) {
            if (sort == null) {
                switch (matcher.group(3)) {
                    case "<":
                    case "desc":
                    case "DESC":
                        sort = Sort.by(matcher.group(1)).descending();
                        break;
                    case ">":
                    case "asc":
                    case "ASC":
                        sort = Sort.by(matcher.group(1)).ascending();
                        break;
                }
            } else {
                switch (matcher.group(3)) {
                    case "<":
                    case "desc":
                    case "DESC":
                        sort = sort.and(Sort.by(matcher.group(1)).descending());
                        break;
                    case ">":
                    case "asc":
                    case "ASC":
                        sort = sort.and(Sort.by(matcher.group(1)).ascending());
                        break;
                }
            }
        }
        return sort;
    }
}
