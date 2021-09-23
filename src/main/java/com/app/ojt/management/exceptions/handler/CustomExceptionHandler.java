package com.app.ojt.management.exceptions.handler;

import com.app.ojt.management.exceptions.CompanyNotExistedException;
import com.app.ojt.management.exceptions.EmptyRoleException;
import com.app.ojt.management.exceptions.UsernameAlreadyExistedException;
import com.app.ojt.management.exceptions.CrudException;
import com.app.ojt.management.exceptions.EmailAlreadyExistedException;
import com.app.ojt.management.exceptions.MajorNotExistedException;
import com.app.ojt.management.payload.response.Response;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

@ControllerAdvice
public class CustomExceptionHandler extends ResponseEntityExceptionHandler {

    @ExceptionHandler(value = {EmailAlreadyExistedException.class, UsernameAlreadyExistedException.class, EmptyRoleException.class,
            CompanyNotExistedException.class, MajorNotExistedException.class})
    protected ResponseEntity<Response> handleAuthExceptions(CrudException exception) {
        return ResponseEntity
                .status(exception.getStatus())
                .body(new Response("Error", exception.getMessage()));
    }

}
