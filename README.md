# fitness-tracker

## Sqitch

On windows

```
.\sqitch add schema.fitness_tracker.create -n 'Add schema.'
.\sqitch.bat verify db:postgres://postgres:guest@host.docker.internal/fitness-tracker
```
