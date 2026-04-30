-- ShinyLabelR — Supabase Schema
-- Run this ONCE in your Supabase project → SQL Editor → New Query → Run
-- ─────────────────────────────────────────────────────────────────────────────

-- 1. USERS
create table if not exists users (
  id           bigserial primary key,
  email        text unique not null,
  display_name text not null,
  role         text not null default 'annotator',  -- 'admin' | 'annotator'
  created_at   timestamptz not null default now()
);

-- 2. INVITE CODES
create table if not exists invite_codes (
  id           bigserial primary key,
  code         text unique not null,
  created_by   bigint references users(id) on delete set null,
  used_by      bigint references users(id) on delete set null,
  expires_at   timestamptz not null,
  used_at      timestamptz
);

-- 3. IMAGES
create table if not exists images (
  id           bigserial primary key,
  filepath     text not null unique,
  filename     text not null,
  img_width    integer not null default 0,
  img_height   integer not null default 0,
  source_type  text not null default 'upload',  -- 'upload' | 'url'
  status       text not null default 'unannotated',  -- 'unannotated' | 'done'
  added_by     text,
  added_at     timestamptz not null default now()
);

-- 4. CLASSES
create table if not exists classes (
  class_id     bigserial primary key,
  class_name   text not null unique,
  color_hex    text not null default '#FF6B6B',
  created_at   timestamptz not null default now()
);

-- 5. ANNOTATIONS
create table if not exists annotations (
  id              bigserial primary key,
  image_id        bigint not null references images(id) on delete cascade,
  class_id        bigint not null references classes(class_id),
  class_name      text not null,
  x_pixel         numeric not null,
  y_pixel         numeric not null,
  w_pixel         numeric not null,
  h_pixel         numeric not null,
  x_center_norm   numeric not null,
  y_center_norm   numeric not null,
  w_norm          numeric not null,
  h_norm          numeric not null,
  annotator_email text not null,
  created_at      timestamptz not null default now(),
  updated_at      timestamptz not null default now()
);

-- ─────────────────────────────────────────────────────────────────────────────
-- Row Level Security (RLS) — disable for anon key (simplest setup for a team
-- tool where you control who has the URL + invite code).
-- If you want per-user RLS in future, enable it here.
-- ─────────────────────────────────────────────────────────────────────────────
alter table users          enable row level security;
alter table invite_codes   enable row level security;
alter table images         enable row level security;
alter table classes        enable row level security;
alter table annotations    enable row level security;

-- Allow all operations via the anon key (your app controls access via invite codes)
create policy "anon_all" on users          for all using (true) with check (true);
create policy "anon_all" on invite_codes   for all using (true) with check (true);
create policy "anon_all" on images         for all using (true) with check (true);
create policy "anon_all" on classes        for all using (true) with check (true);
create policy "anon_all" on annotations    for all using (true) with check (true);
