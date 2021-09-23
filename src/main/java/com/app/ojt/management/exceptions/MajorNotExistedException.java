package com.app.ojt.management.exceptions;

import org.springframework.http.HttpStatus;

public class MajorNotExistedException extends CrudException {
    public MajorNotExistedException() {
        super("Major does not exist", HttpStatus.BAD_REQUEST);
    }
}
