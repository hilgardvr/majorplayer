create table if not exists sessions (
    id uuid primary key default uuid_generate_v4(),
    user_id uuid not null references users(id),
    session_id uuid not null default uuid_generate_v4(),
    expiry timestamp
)
