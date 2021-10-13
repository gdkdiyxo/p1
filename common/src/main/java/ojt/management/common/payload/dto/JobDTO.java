package ojt.management.common.payload.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import ojt.management.data.entities.Major;
import ojt.management.data.entities.Semester;

import java.io.Serializable;
import java.util.Set;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class JobDTO implements Serializable {
    private Long id;
    private String name;
    private String description;
    private String title;
    private Set<Semester> semesters;
    private Set<Major> major;
    private CompanyDTO company;
}
