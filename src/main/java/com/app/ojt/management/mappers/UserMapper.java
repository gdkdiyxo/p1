package com.app.ojt.management.mappers;

import com.app.ojt.management.entities.Account;
import com.app.ojt.management.payload.dto.UserDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

@Mapper(componentModel = "spring")
public interface UserMapper {
    @Mapping(source = "account", target = "role", qualifiedByName = "role")
    UserDTO userToUserDTO(Account account);

    Account userDTOToUser(UserDTO userDTO);

    @Named(("role"))
    default String convertRole(Account account){
        String role;
        if (account.isAdmin()) {
            role = "SYS_ADMIN";
        } else if (account.getRepresentative() != null && account.getStudent() == null) {
            role = "COMPANY_REPRESENTATIVE";
        } else {
            role = "STUDENT";
        }
        return role;
    }

}
