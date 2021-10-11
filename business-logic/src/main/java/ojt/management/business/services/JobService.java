package ojt.management.business.services;

import ojt.management.common.exceptions.CrudException;
import ojt.management.common.exceptions.JobNotExistedException;
import ojt.management.common.payload.request.JobCreateRequest;
import ojt.management.common.payload.request.JobUpdateRequest;
import ojt.management.data.entities.Job;
import org.springframework.security.core.Authentication;

import java.util.List;

public interface JobService {

    List<Job> searchJobs(String name, String title, Long semesterId, Long majorId, Authentication authentication);

    Job getById(Long id, Authentication authentication) throws JobNotExistedException;

    Job updateJob(JobUpdateRequest jobUpdateRequest, Authentication authentication) throws CrudException;

    boolean deleteJob(Long id, Authentication authentication) throws JobNotExistedException;

    Job createJob(JobCreateRequest jobCreateRequest, Authentication authentication) throws CrudException;
}
