package ojt.management.common.utils;

import org.springframework.data.domain.Sort;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SortUtils {
    public static Sort parseSortQuery(String query){
        Pattern pattern = Pattern.compile("(\\w+?)(<|>),");
        Matcher matcher = pattern.matcher(query + ",");
        Sort sort = null;
        while (matcher.find()) {
            if(sort == null){
                switch(matcher.group(2)){
                    case "<":
                        sort = Sort.by(matcher.group(1)).descending();
                        break;
                    case ">":
                        sort = Sort.by(matcher.group(1)).ascending();
                        break;
                }
            }else{
                switch(matcher.group(2)){
                    case "<":
                        sort = sort.and(Sort.by(matcher.group(1)).descending());
                        break;
                    case ">":
                        sort = sort.and(Sort.by(matcher.group(1)).ascending());
                        break;
                }
            }
        }
        return sort;
    }
}
