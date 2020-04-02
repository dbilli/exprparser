
from setuptools import setup, find_packages

install_requires = [
    "pyparsing"
]

setup(
    name="exprparser",
    version="0.1",
    packages=find_packages(),

    install_requires=install_requires,

    author="Diego Billi",
    author_email="diegobilli@gmail.com",
    description="SNMP Expression Parser",
    keywords="Sanet snmp language parser",
    url="https://github.com/dbilli/exprparser/",
    project_urls={
        "Bug Tracker"  : "https://github.com/dbilli/yaesql/issues",
        "Documentation": "https://github.com/dbilli/yaesql/",
    },
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU General Public License v2 (GPLv2)",
    ],
    
    test_suite="tests.run_tests",
)
