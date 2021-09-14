package com.app.source.exceptions;

import org.springframework.http.HttpStatus;

public class EmailAlreadyExistedException extends CrudException {
    public EmailAlreadyExistedException() {
        super("Error: Email is already in use!", HttpStatus.BAD_REQUEST);
    }
}
