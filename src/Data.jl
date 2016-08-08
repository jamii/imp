module Data

using Imp

function read_chinook(filename, types; comments=false)
  rows, _ = readdlm(open(filename), '\t', header=true, quotes=false, comments=comments)
  n = length(types)
  columns = tuple([Vector{types[c]}() for c in 1:n]...)
  for r in 1:size(rows)[1]
    for c in 1:n
      if types[c] == String
        push!(columns[c], string(rows[r, c]))
      elseif isa(rows[r, c], SubString)
        @show r c rows[r, c]
      else
        push!(columns[c], rows[r, c])
      end
    end
  end
  Relation(columns)
end

album = read_chinook("data/Album.csv", [Int64, String, Int64])
artist = read_chinook("data/Artist.csv", [Int64, String])
track = read_chinook("data/Track.csv", [Int64, String, Int64, Int64, Int64, String, Int64, Int64, Float64])
playlist_track = read_chinook("data/PlaylistTrack.csv", [Int64, Int64])
playlist = read_chinook("data/Playlist.csv", [Int64, String])

export album, artist, track, playlist_track, playlist

using DataFrames

function read_job()
  schema = readdlm(open("data/job_schema.csv"), ',', header=false, quotes=true, comments=false)
  table_column_names = Dict()
  table_column_types = Dict()
  for column in 1:size(schema)[1]
    table_name, ix, column_name, column_type = schema[column, 1:4]
    push!(get!(table_column_names, table_name, []), column_name)
    push!(get!(table_column_types, table_name, []), (column_type == "integer" ? Int64 : String))
  end
  relations = []
  names = []
  for (table_name, column_names) in table_column_names
    if isfile("../job/$(table_name).csv")
      column_types = table_column_types[table_name]
      @show table_name column_names column_types
      frame = readtable(open("../imdb/$(table_name).csv"), header=false, eltypes=column_types)
      n = length(frame[1])
      ids = copy(frame[1].data)
      for (ix, (column_name, column_type)) in enumerate(zip(column_names, column_types))
        @show table_name ix column_name column_type
        data_array = frame[ix]
        if ix == 1
          push!(names, symbol(table_name))
          push!(relations, Relation((ids,)))
        else
          column_ids = Int64[id for (ix, id) in enumerate(ids) if !(data_array.na[ix])]
          local column
          if isa(data_array, DataArray{Int64})
            let data::Vector{Int64} = data_array.data
              column = Int64[d for (ix, d) in enumerate(data) if !(data_array.na[ix])]
            end
          elseif isa(data_array, DataArray{String})
            let data::Vector{String} = data_array.data
              column = String[d for (ix, d) in enumerate(data_array.data) if !(data_array.na[ix])]
            end
          end
          push!(names, symbol(table_name, "_", column_name))
          push!(relations, Relation((column_ids, column)))
        end
      end
    end
  end
  (names, relations)
end

# (names, relations) = @time read_job()
# 
# open("../job/imp.bin", "w") do f
#     @time serialize(f, (names, relations))
# end

# (names, relations) = deserialize(open("../job/imp.bin"))

# person_info,person_info_person_id,person_info_info_type_id,person_info_info,person_info_note,title,title_title,title_imdb_index,title_kind_id,title_production_year,title_imdb_id,title_phonetic_code,title_episode_of_id,title_season_nr,title_episode_nr,title_series_years,title_md5sum,link_type,link_type_link,cast_info,cast_info_person_id,cast_info_movie_id,cast_info_person_role_id,cast_info_note,cast_info_nr_order,cast_info_role_id,movie_info_idx,movie_info_idx_movie_id,movie_info_idx_info_type_id,movie_info_idx_info,movie_info_idx_note,name,name_name,name_imdb_index,name_imdb_id,name_gender,name_name_pcode_cf,name_name_pcode_nf,name_surname_pcode,name_md5sum,info_type,info_type_info,aka_name,aka_name_person_id,aka_name_name,aka_name_imdb_index,aka_name_name_pcode_cf,aka_name_name_pcode_nf,aka_name_surname_pcode,aka_name_md5sum,movie_info,movie_info_movie_id,movie_info_info_type_id,movie_info_info,movie_info_note,role_type,role_type_role,aka_title,aka_title_movie_id,aka_title_title,aka_title_imdb_index,aka_title_kind_id,aka_title_production_year,aka_title_phonetic_code,aka_title_episode_of_id,aka_title_season_nr,aka_title_episode_nr,aka_title_note,aka_title_md5sum,complete_cast,complete_cast_movie_id,complete_cast_subject_id,complete_cast_status_id,movie_keyword,movie_keyword_movie_id,movie_keyword_keyword_id,kind_type,kind_type_kind,movie_link,movie_link_movie_id,movie_link_linked_movie_id,movie_link_link_type_id,company_name,company_name_name,company_name_country_code,company_name_imdb_id,company_name_name_pcode_nf,company_name_name_pcode_sf,company_name_md5sum,keyword,keyword_keyword,keyword_phonetic_code,comp_cast_type,comp_cast_type_kind,char_name,char_name_name,char_name_imdb_index,char_name_imdb_id,char_name_name_pcode_nf,char_name_surname_pcode,char_name_md5sum,movie_companies,movie_companies_movie_id,movie_companies_company_id,movie_companies_company_type_id,movie_companies_note,company_type,company_type_kind = relations
# 
# export person_info,person_info_person_id,person_info_info_type_id,person_info_info,person_info_note,title,title_title,title_imdb_index,title_kind_id,title_production_year,title_imdb_id,title_phonetic_code,title_episode_of_id,title_season_nr,title_episode_nr,title_series_years,title_md5sum,link_type,link_type_link,cast_info,cast_info_person_id,cast_info_movie_id,cast_info_person_role_id,cast_info_note,cast_info_nr_order,cast_info_role_id,movie_info_idx,movie_info_idx_movie_id,movie_info_idx_info_type_id,movie_info_idx_info,movie_info_idx_note,name,name_name,name_imdb_index,name_imdb_id,name_gender,name_name_pcode_cf,name_name_pcode_nf,name_surname_pcode,name_md5sum,info_type,info_type_info,aka_name,aka_name_person_id,aka_name_name,aka_name_imdb_index,aka_name_name_pcode_cf,aka_name_name_pcode_nf,aka_name_surname_pcode,aka_name_md5sum,movie_info,movie_info_movie_id,movie_info_info_type_id,movie_info_info,movie_info_note,role_type,role_type_role,aka_title,aka_title_movie_id,aka_title_title,aka_title_imdb_index,aka_title_kind_id,aka_title_production_year,aka_title_phonetic_code,aka_title_episode_of_id,aka_title_season_nr,aka_title_episode_nr,aka_title_note,aka_title_md5sum,complete_cast,complete_cast_movie_id,complete_cast_subject_id,complete_cast_status_id,movie_keyword,movie_keyword_movie_id,movie_keyword_keyword_id,kind_type,kind_type_kind,movie_link,movie_link_movie_id,movie_link_linked_movie_id,movie_link_link_type_id,company_name,company_name_name,company_name_country_code,company_name_imdb_id,company_name_name_pcode_nf,company_name_name_pcode_sf,company_name_md5sum,keyword,keyword_keyword,keyword_phonetic_code,comp_cast_type,comp_cast_type_kind,char_name,char_name_name,char_name_imdb_index,char_name_imdb_id,char_name_name_pcode_nf,char_name_surname_pcode,char_name_md5sum,movie_companies,movie_companies_movie_id,movie_companies_company_id,movie_companies_company_type_id,movie_companies_note,company_type,company_type_kind

end
