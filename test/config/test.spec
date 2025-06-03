{logdir, "log"}.
{config, "test.config"}.
{define, 'TestDir', ".."}.
{suites, 'TestDir', all}.
{ct_hooks, [{cth_surefire, [{path, "report.xml"}]}]}.