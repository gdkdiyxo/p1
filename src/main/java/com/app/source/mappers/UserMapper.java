package com.app.source.mappers;

import com.app.source.entities.Account;
import com.app.source.payload.dto.UserDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.Named;


@Mapper(componentModel = "spring")
public interface UserMapper {
    @Mapping(source = "account", target = "role", qualifiedByName = "role")
    UserDTO userToUserDTO(Account account);

    Account userDTOToUser(UserDTO userDTO);


    @Named(("roleName"))
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
