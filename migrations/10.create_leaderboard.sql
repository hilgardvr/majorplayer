create table if not exists leaderboard (
    id uuid primary key default uuid_generate_v4(),
    raw_response varchar not null,
    created_at timestamp default now()
)
