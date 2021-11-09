package ojt.management.seeding;

import ojt.management.data.entities.Job;

import java.util.Arrays;
import java.util.List;

public class JobData {
    public static List<Job> getSeedData() {
        List<Job> jobs = Arrays.asList(
                new Job("Java Developer (All Levels) - Up to $2k", "Java Developer", "800 - 2,000 USD",
                        Arrays.asList("Flexible working time, no OT", "KPI Incentive~15monthly salary", "Premium Insurance for Family"),
                        "We are seeking a motivated Technical Architect (Java/NodeJs)  " +
                                "who is passionate about great quality digital products and willing to work on the entire stack, " +
                                "from DevOps, API to front-end and even testing automation. You love solving problems, have the ability to work independently " +
                                "but even better in a team and you have a strong thirst to learn new technology and frameworks. " +
                                "We are looking for great team players that share their knowledge and are willing to do what it takes to deliver.",
                        Arrays.asList("Design and develop mobile applications for Android (Kotlin, Java).",
                                "Analyze, design, program, test, document and maintain mobile applications",
                                "Provide maintenance support to customers (internal and/or external) by investigating and rectifying reported system shortcomings.",
                                "Participate in cutting-edge digital project development for worldwide leading companies on various business domains.",
                                "Analyze requirements, come up with adequate technical design and maintain technical documentation.",
                                "Collaborate closely with Project Manager and other team members to raise up suggestions or any concerns; actively suggest a plan of actions to improve overall quality."),
                        "Our team is responsible for creating a brand of new digital financial platforms and apps that are highly reliable and scalable " +
                                "using modern engineering practices. You will be joining a project which talented software engineers in our Digital Bank Build team who " +
                                "is responsible for designing, building and maintaining the new digital banking platform and the customer channels. " +
                                "As part of the team, you will be responsible for implementing software features, involve in technical designs and writing tests to ensure " +
                                "the high quality delivery of the product. The teams operate in Scrum and DevOps model. " +
                                "We're looking for top engineers out there! During the interview process we will test your coding and design skills to assess your " +
                                "experience and depth of knowledge. Don't worry our interview process will be fun!",
                        Arrays.asList("As a backend engineer, you will be working within a specific problem where you will design, develop, and deploy backend services with a focus on scalability, high availability, and low latency.",
                                "Solving complex technical and business problems and learn new technology and frameworks",
                                "Be part of a team that will take full responsibility for the features you own.",
                                "Design, develop, test, deploy, monitor and improve, you are responsible for the full life-cycle of your product – build it, own it."),
                        Arrays.asList("Outstanding problem-solving ability, eagerness to learn, and curiosity.",
                                "A few years of software development experience with one or more general-purpose programming languages.",
                                "Strong database and schema design for large-scale application.",
                                "Adaptable attitude and personality that is ready for continuous change.",
                                "Collaboration and culture fit in Agile experience will be an advantage.",
                                "Good English reading and writing skills",
                                "Experience working in the banking and the financial domain is a plus"),
                        Arrays.asList("Experience in developing distributed systems on top of micro-services architecture, event-driven architecture using Java, Spring and Sprint boot, Kafka, Redis, etc. is a big plus",
                                "Experience in AWS, Ansible, Packer, Docker, Rancher, K8s is a big plus",
                                "Experienced in automated testing frameworks is a plus",
                                "Good English listening and speaking is a big plus"),
                        "We pride ourselves on being cutting edge and stretching the limits of all our personnel - whether you are a mobile engineer, " +
                                "in quality assurance or a technical lead architect in our team. Our engineering teams form a cohesive, collaborative and " +
                                "motivated community that delivers innovative solutions to our businesses in Asia and Africa.",
                        Arrays.asList("Meal and parking allowance covered by company.",
                                "Full benefits and salary rank during probation.",
                                "Insurances as Vietnamese labor law and premium health care for you and your family.",
                                "SMART goals and clear career opportunities (technical seminar, conference and career talk) - we focus on your development.",
                                "Values driven, international working environment and agile culture.",
                                "Overseas travel opportunities for training and working related.",
                                "Internal Hackathons and company's events (team building, coffee run, blue card...)",
                                "Pro-Rate and performance bonus.",
                                "15-day annual + 3-day sick leave per year from company.",
                                "Work-life balance 40-hr per week from Mon to Fri.")
                )
        );
        return jobs;
    }
}
