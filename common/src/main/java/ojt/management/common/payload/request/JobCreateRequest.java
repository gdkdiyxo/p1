package ojt.management.common.payload.request;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import ojt.management.data.entities.Major;
import ojt.management.data.entities.Semester;

import java.util.Set;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class JobCreateRequest {
    private String name;
    private String description;
    private String title;
    private Set<Semester> semesters;
    private Set<Major> majors;
}
