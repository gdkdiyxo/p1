package com.app.source.mappers;

import com.app.source.entities.ApplicationUser;
import com.app.source.payload.dto.UserDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface UserMapper {
    @Mapping(source = "role", target = "role")
    UserDTO userToUserDTO(ApplicationUser applicationUser);

    ApplicationUser userDTOToUser(UserDTO userDTO);
}
