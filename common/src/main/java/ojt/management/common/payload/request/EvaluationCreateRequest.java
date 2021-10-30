package ojt.management.common.payload.request;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class EvaluationCreateRequest implements Serializable{
    @NotNull
    @NotBlank
    @Size(max = 500)
    private String comment;

    @NotNull
    private Long grade;

    @NotNull
    private Long applicationId;
}
