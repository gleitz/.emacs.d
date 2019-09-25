import os
import sys


def get_os_result(command):
    return os.popen(command).readline().strip()


def get_git_root_directory(path):
    if not path or len(path) == 1:
        return False
    dir_name = os.path.dirname(path)
    git_path = os.path.join(dir_name, '.git')
    if os.path.exists(git_path):
        return dir_name
    return get_git_root_directory(dir_name)


def get_github_address():
    url = get_os_result('git config --get remote.origin.url')
    service = 'github' if 'github' in url else 'gitlab'
    url = url[url.find(service):url.rfind('.git')]
    url = url.replace(':', '/')
    return url


def get_branch_name():
    return get_os_result('git rev-parse --abbrev-ref HEAD')


def get_github_url(filename, start_line=False, end_line=False):
    github_address = get_github_address()
    branch_name = get_branch_name()
    git_root_directory = get_git_root_directory(filename)
    if not all([github_address, branch_name, git_root_directory]):
        return False
    relative_path = filename.replace(git_root_directory, '')
    if os.path.isdir(filename):
        url_str = 'http://{0}/tree/{1}{2}'
    else:
        url_str = 'http://{0}/blob/{1}{2}'
    complete_url = url_str.format(github_address,
                                  branch_name,
                                  relative_path)
    if start_line and start_line != 1:
        complete_url = '{0}#L{1}'.format(complete_url, start_line)
        if end_line:
            complete_url = complete_url + '-L{0}'.format(end_line)
    return complete_url



def main():
    """ Usage: python gitopener.py (optional: /path/to/file, default: cwd)"""
    start_line = None
    end_line = None
    if len(sys.argv) > 3:
        filename = sys.argv[1]
        start_line = int(sys.argv[2])
        end_line = int(sys.argv[3])
    elif len(sys.argv) > 2:
        filename = sys.argv[1]
        start = sys.argv[2]
    elif (sys.argv) > 1:
        filename = sys.argv[1]
    else:
        filename = os.getcwd()
    current_dir = filename
    if not os.path.isdir(filename):
        current_dir = os.path.dirname(filename)
    os.chdir(current_dir)
    print get_github_url(filename, start_line, end_line)


if __name__ == '__main__':
    main()
