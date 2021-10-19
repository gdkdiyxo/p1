package ojt.management.common.payload.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ApplicationDTO {
    private Long id;
    private String experience;
    private boolean isCompanyAccepted;
    private boolean isStudentConfirmed;
    private StudentDTO student;
    private JobDTO job;
}
