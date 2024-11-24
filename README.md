# Template dataproject
A template for data projects

Custom project templates would be nice, but is a premium feature.
https://docs.gitlab.com/ee/user/group/custom_project_templates.html


# Howto 

1. To develop locally, specify `DIR_raw_data` as `/data/shared/...` where ... is the relevant data path
2. In the gitlab-ci pipeline, `DIR_raw_data` is specified automatically for you (`/data/local/...`)
3. Make all results/plots with `main.R` or `main.py`
4. Save all results/plots which should be accessible later in `./res/folder_A`, `./res/folder_B`. 
For every subfolder in `res` there will be a downloadable, compressed file. 

# Info
The local `.gitlab-ci.yml` uses `datasrc/baseproject/.gitlab-ci.yml` as basis, you just have to specify which version of the baseproject you want to use by using its respective commit-hash. 

