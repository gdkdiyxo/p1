package ojt.management.common.payload.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class JobCreateRequest extends JobRequest implements Serializable {
    @NotNull
    private Long companyId;

    public JobCreateRequest(String name,
                            String title,
                            String description,
                            String skills,
                            String benefits,
                            List<Long> semesterIds,
                            List<Long> majorIds,
                            Long companyId) {
        super(name, title, description, skills, benefits, semesterIds, majorIds);
        this.companyId = companyId;
    }
}
