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
public class AccountCompanyUpdateRequest extends AccountUpdateRequest implements Serializable {
    @NotNull
    @NotBlank
    @Size(max = 255)
    private String companyName;

    @NotNull
    @NotBlank
    @Size(max = 500)
    private String companyDescription;
}
