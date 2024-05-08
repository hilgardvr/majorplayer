create table if not exists team (
    id uuid primary key default uuid_generate_v4(),
    user_id uuid not null references users(id),
    golfer_ids int[] not null,
    tournament_id int not null,
    captain_id int not null,
    constraint constraint_uni_user_tournament_team UNIQUE (user_id, tournament_id)
)
