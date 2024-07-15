# fitness-tracker

## Sqitch

On windows

```
.\sqitch add schema.fitness_tracker.create -n 'Add schema.'
.\sqitch.bat verify db:postgres://postgres:guest@host.docker.internal:5432/fitness-tracker
.\sqitch.bat deploy db:postgres://postgres:guest@host.docker.internal:5432/fitness-tracker
```
