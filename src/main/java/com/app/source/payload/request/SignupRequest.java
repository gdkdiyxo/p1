package com.app.source.payload.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class SignupRequest {
    private String email;
    private String password;
    private String firstName;
    private String lastName;
    private String role;
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
    private String address;
    private String studentCode;
    private Long majorId;

    /**
     * End of student Section
     **/


    // Constructor or admin creation
    public SignupRequest(String email, String password, String firstName, String lastName, String role, String phone) {
        this.email = email;
        this.password = password;
        this.firstName = firstName;
        this.lastName = lastName;
        this.role = role;
        this.phone = phone;
    }
}
