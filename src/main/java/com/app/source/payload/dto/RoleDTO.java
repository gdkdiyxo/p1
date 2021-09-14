package com.app.source.payload.dto;

import com.app.source.enums.RoleEnum;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class RoleDTO {
    private Integer id;
    private RoleEnum name;
}
