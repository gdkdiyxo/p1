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
public class ApplicationRequest implements  Serializable{
    @NotNull
    @NotBlank
    @Size(max = 1000)
    private String experience;

    @NotNull
    private Long jobId;

    @NotNull
    private Long accountId;

    @NotNull
    private boolean isCompanyAccepted;

    @NotNull
    private boolean isStudentConfirmed;
}
