create table if not exists golfer_rankings (
    id uuid primary key default uuid_generate_v4(),
    raw_response varchar not null,
    created_at timestamp default now()
)
