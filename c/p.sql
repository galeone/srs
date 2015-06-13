CREATE OR REPLACE FUNCTION hashtag(message text, hpid bigint, grp boolean) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare field text;
BEGIN
    IF grp THEN
        field := 'g_hpid';
    ELSE
        field := 'u_hpid';
    END IF;

    message = quote_literal(message);

    EXECUTE '
    insert into posts_classification(' || field || ' , tag)
    select distinct ' || hpid ||', tmp.matchedTag[1] from (
        -- 1: existing hashtags
       select regexp_matches(' ||
         regexp_replace(regexp_replace(
         regexp_replace(regexp_replace(
         regexp_replace(regexp_replace(
         regexp_replace(regexp_replace(message,
            '\[url[^\]]*\](.+?)\[/url\]',' \1 ','gi'),
            '\[code=[^\]]+\].+?\[/code\]',' ','gi'),
            '\[video\].+?\[/video\]',' ','gi'),
            '\[yt\].+?\[/yt\]',' ','gi'),
            '\[youtube\].+?\[/youtube\]',' ','gi'),
            '\[music\].+?\[/music\]',' ','gi'),
            '\[img\].+?\[/img\]',' ','gi'),
            '\[twitter\].+?\[/twitter\]',' ','gi')
         || ', ''(#(?!\d+[\W])[\w]{1,34})'', ''gi'')
        as matchedTag
            union distinct -- 2: spoiler
        select concat(''{#'', a.matchedTag[1], ''}'')::text[] from (
            select regexp_matches(' || message || ', ''\[spoiler=([\w]{1,34})\]'', ''gi'')
            as matchedTag
        ) as a
            union distinct -- 3: languages
         select concat(''{#'', b.matchedTag[1], ''}'')::text[] from (
             select regexp_matches(' || message || ', ''\[code=([\w]{1,34})\]'', ''gi'')
            as matchedTag
        ) as b
    ) tmp
    where not exists (
        select 1 from posts_classification p where ' || field ||'  = ' || hpid || ' and p.tag = tmp.matchedTag[1]
    )
    ';
END $$;

delete from posts_classification where id in (select id from (select regexp_matches(tag ,'^#\d+$', 'g'), id from posts_classification) t);

