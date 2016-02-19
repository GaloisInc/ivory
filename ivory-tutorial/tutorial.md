# Ivory Tutorial

## Environmental Setup

Goals:
* Install stack
* Checkout the ivory repository
* Use stack to configure an ivory build environment
* Setup a skeleton project and get it building

### Installing stack

Follow the directions for your OS at [haskellstack.org](http://docs.haskellstack.org/en/stable/README/#how-to-install)

### Checkout the ivory repository

If you have git installed, you can just issue the following command:
```sh
$ git clone https://github.com/galoisinc/ivory
```

If not, just click the `download zip` button on the [ivory github page](https://github.com/galoisinc/ivory),
and unzip the archive.

### Configure the build environment

Using a terminal, change to the checked out or unzipped ivory repository. Next, to make sure that we're using version `7.10` of GHC, make the following change: copy the `stack-7.10.yaml` file over the `stack.yaml` file that already exists. Now, simply run `stack build` to setup the build environment.

```sh
$ cp stack-7.10.yaml stack.yaml
$ stack build
```

## Motivating Example
