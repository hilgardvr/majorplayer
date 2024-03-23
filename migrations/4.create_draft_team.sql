create table if not exists draft_team (
    id uuid primary key default uuid_generate_v4(),
    user_id uuid not null references users(id),
    golfer_id int not null,
    constraint constraint_uni_user_golfer UNIQUE (user_id, golfer_id)
)
