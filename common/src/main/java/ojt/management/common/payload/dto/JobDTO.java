package ojt.management.common.payload.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;
import java.util.Set;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class JobDTO implements Serializable {
    private Long id;
    private String name;
    private String title;
    private String description;
    private List<String> benefits;
    private Set<SemesterDTO> semesters;
    private Set<MajorDTO> majors;
    private CompanyDTO company;
}
