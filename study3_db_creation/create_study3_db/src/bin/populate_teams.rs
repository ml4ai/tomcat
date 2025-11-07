use glob::glob;

fn main() {
    for entry in glob("/media/mule/projects/tomcat/protected/study-3_2022/HSRData*.metadata")
        .into_iter()
        .flatten()
    {
        let path = entry.unwrap().into_os_string().into_string().unwrap();
        let parts = path.split("/");
        for part in parts {
            println!("{:?}", part)
        }
    }
}
