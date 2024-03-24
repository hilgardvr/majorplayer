create table if not exists league_users (
    league_id uuid not null references leagues(id),
    user_id uuid not null references users(id),
    constraint constraint_uni_user_league UNIQUE (league_id, user_id)
)
