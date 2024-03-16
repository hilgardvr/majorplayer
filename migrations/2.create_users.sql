create table if not exists users (
    id uuid primary key default uuid_generate_v4(),
    email varchar not null
)
