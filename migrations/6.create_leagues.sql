create table if not exists leagues (
    id uuid primary key default uuid_generate_v4(),
    admin_id uuid not null references users(id),
    name varchar not null,
    passcode varchar,
    constraint constraint_uni_admin_league_name UNIQUE (admin_id, name)
)
