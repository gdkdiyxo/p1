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
    @NotBlank
    private Long id;

    @NotNull
    @Size(min = 10, max = 13)
    private String phone;

    @NotNull
    @NotBlank
    @Size(min = 8, max = 1024)
    private String password;

    @NotNull
    @NotBlank
    @Size(max = 255)
    private String name;

    @Email
    @NotNull
    @NotBlank
    @Size(max = 320)
    private String email;

    //    ===== [Start] Update for Student only =====
    @Size(max = 500)
    private String address;

    @NotNull
    @NotBlank
    @Size(max = 10)
    private String studentCode;

    @NotNull
    private Long majorId;

    @NotNull
    private Long semesterId;
    //    ===== [End] Update for Student only =====

    //    ===== [Start] Update for Company only =====
    @NotNull
    @NotBlank
    @Size(max = 255)
    private String companyName;

    @NotNull
    @NotBlank
    @Size(max = 500)
    private String companyDescription;
    //    ===== [End] Update for Company only =====

}
