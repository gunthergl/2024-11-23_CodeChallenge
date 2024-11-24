dir.create("intermediate")
if (!file.exists("intermediate/freMTPL2freq.arff")) {
    download.file("https://www.openml.org/data/download/20649148/freMTPL2freq.arff", "intermediate/freMTPL2freq.arff")
}
if (!file.exists("intermediate/freMTPL2sev.arff")) {
    download.file("https://www.openml.org/data/download/20649149/freMTPL2sev.arff", "intermediate/freMTPL2sev.arff")
}
