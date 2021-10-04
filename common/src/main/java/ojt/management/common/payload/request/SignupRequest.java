package ojt.management.common.payload.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class SignupRequest {
    @Email
    @Max(320)
    @NotNull
    private String email;

    @NotNull
    @Max(1024)
    @Min(8)
    private String password;

    @NotNull
    @Max(255)
    @NotBlank
    private String name;

    @NotNull
    @NotBlank
    private String role;

    @NotNull
    @Max(13)
    @Min(10)
    private String phone;

    /**
     * Start of company Section
     **/
    private Long companyId;
    /**
     * End of company Section
     **/

    /**
     * Start of student Section
     **/
    @NotBlank
    @Max(500)
    @NotNull
    private String address;

    @Max(10)
    @NotBlank
    @NotNull
    private String studentCode;

    private Long majorId;

    /**
     * End of student Section
     **/


    // Constructor or admin creation
    public SignupRequest(String email, String password, String name, String role, String phone) {
        this.email = email;
        this.password = password;
        this.name = name;
        this.role = role;
        this.phone = phone;
    }

    // constructor for representative
    public SignupRequest(String email, String password, String name, String role, String phone, Long companyId) {
        this.email = email;
        this.password = password;
        this.name = name;
        this.role = role;
        this.phone = phone;
        this.companyId = companyId;
    }

    // constructor for student
    public SignupRequest(String email, String password, String name, String role, String phone, String address, String studentCode, Long majorId) {
        this.email = email;
        this.password = password;
        this.name = name;
        this.role = role;
        this.phone = phone;
        this.address = address;
        this.studentCode = studentCode;
        this.majorId = majorId;
    }
}
