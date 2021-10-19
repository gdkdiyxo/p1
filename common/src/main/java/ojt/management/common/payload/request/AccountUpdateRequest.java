package ojt.management.common.payload.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class AccountUpdateRequest implements Serializable {
    @NotNull
    private Long id;

    @Email
    @NotNull
    @NotBlank
    @Size(max = 320)
    private String email;

    @NotNull
    @NotBlank
    @Size(max = 255)
    private String name;

    @NotNull
    @Size(min = 10, max = 13)
    private String phone;

    @NotNull
    @NotBlank
    @Size(min = 8, max = 1024)
    private String password;
}
