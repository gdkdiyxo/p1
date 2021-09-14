package com.app.source.mappers;

import com.app.source.entities.Role;
import com.app.source.payload.dto.RoleDTO;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface RoleMapper {
    Role roleDTOToRole(RoleDTO roleDTO);

    RoleDTO roleToRoleDTO(Role role);
}
