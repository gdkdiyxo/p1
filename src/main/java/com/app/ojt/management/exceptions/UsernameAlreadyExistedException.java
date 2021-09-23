package com.app.ojt.management.exceptions;

import org.springframework.http.HttpStatus;

public class UsernameAlreadyExistedException extends CrudException {
    public UsernameAlreadyExistedException() {
        super("Error: Username is already taken!", HttpStatus.BAD_REQUEST);
    }
}
