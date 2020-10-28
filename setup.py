#!/usr/bin/env python
import os
from setuptools import setup, find_packages

with open(os.path.join(os.path.dirname(__file__),'srcpy','README.md')) as f:
    readme = f.read()

with open('LICENSE') as f:
    license = f.read()


pyLPT_cmds = ['pyLPT = pyLPT.__main__:main']

setup(
    name='pyLPT',
    version='0.1.0',
    description='The LPT Python interface for accessing LPT\'s variables'\
                'and subroutines in Python',
    keywords='LPT, Python, interface, f2py',
    long_description=readme,
    author='Gajanan Choudhary',
    author_email='gajananchoudhary91@gmail.com',
    url='https://github.com/gajanan-choudhary/LPT',
    license=license,
    entry_points={'console_scripts': pyLPT_cmds},
    package_dir={'': '.'},
    package_data={'': ['liblpt_shared.so', 'pylpt*.so']},
    packages=find_packages(exclude=('src', 'srcpy', 'tests', 'doc', 'cmake',
        'build')),
)
