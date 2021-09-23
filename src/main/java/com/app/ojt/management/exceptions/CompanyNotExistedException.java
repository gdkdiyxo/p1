package com.app.ojt.management.exceptions;

import org.springframework.http.HttpStatus;

public class CompanyNotExistedException extends CrudException {
    public CompanyNotExistedException() {
        super("Company does not exist", HttpStatus.BAD_REQUEST);
    }
}
