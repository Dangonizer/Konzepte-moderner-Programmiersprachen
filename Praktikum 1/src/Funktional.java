import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;

public class Funktional {

    public void mainn(String[] args) {
        String file_path = ("alben.xml");

        byte[] file_contents = null;
        try {
            file_contents = Files.readAllBytes(Paths.get(file_path));
        } catch (Exception e) {
            e.printStackTrace();
        }

        ArrayList<String> tokenList = createTokenList(file_contents);
        ArrayList<Album> albums = parseFile(tokenList);
        for (Album album : albums) {
            System.out.println(album.toString());
        }
    }

    public ArrayList<String> createTokenList(byte[] file_contents) {
        return createTokenListHelper(file_contents, 0, new ArrayList<String>());
    }

    private ArrayList<String> createTokenListHelper(byte[] file_contents, int current_character,
            ArrayList<String> tokenList) {
        if (current_character >= file_contents.length) {
            return tokenList;
        }
        if (file_contents[current_character] == '\n' || file_contents[current_character] == '\r'
                || file_contents[current_character] == '\t') {
            return createTokenListHelper(file_contents, current_character + 1, tokenList);
        } else if (file_contents[current_character] == '<') {
            Object[] returnVals = getToken(file_contents, current_character + 1, "");
            tokenList.add((String) returnVals[0]);
            return createTokenListHelper(file_contents, (int) returnVals[1], tokenList);
        } else {
            Object[] returnVals = getToken(file_contents, current_character, "");
            tokenList.add((String) returnVals[0]);
            return createTokenListHelper(file_contents, (int) returnVals[1], tokenList);
        }
    }

    private Object[] getToken(byte[] file_contents, int current_character, String token) {
        if (file_contents[current_character] == '>' || file_contents[current_character] == '<') {
            return new Object[] { token, current_character + 1 };
        } else {
            return getToken(file_contents, current_character + 1,
                    token + new String(file_contents, current_character, 1, StandardCharsets.UTF_8));
        }
    }

    public ArrayList<Album> parseFile(ArrayList<String> tokenList) {
        return parseFileHelper(tokenList, 0, new ArrayList<Album>(), false, false);
    }

    private ArrayList<Album> parseFileHelper(ArrayList<String> tokenList, int current_token, ArrayList<Album> albums,
            boolean inAlbum, boolean inTrack) {
        if (current_token >= tokenList.size()) {
            return albums;
        }
        if (tokenList.get(current_token).equals("album")) {
            albums.add(new Album());
            return parseFileHelper(tokenList, current_token + 1, albums, true, false);
        } else if (tokenList.get(current_token).equals("/album")) {
            return parseFileHelper(tokenList, current_token + 1, albums, false, false);
        } else if (tokenList.get(current_token).equals("title")) {
            if (!inTrack) {
                albums.getLast().title = tokenList.get(current_token + 1);
            } else {
                albums.getLast().tracks.getLast().title = tokenList.get(current_token + 1);
            }
            return parseFileHelper(tokenList, current_token + 3, albums, inAlbum, inTrack);
        } else if (tokenList.get(current_token).equals("date")) {
            albums.getLast().date = tokenList.get(current_token + 1);
            return parseFileHelper(tokenList, current_token + 3, albums, inAlbum, inTrack);
        } else if (tokenList.get(current_token).equals("artist")) {
            albums.getLast().artist = tokenList.get(current_token + 1);
            return parseFileHelper(tokenList, current_token + 3, albums, inAlbum, inTrack);
        } else if (tokenList.get(current_token).equals("track")) {
            albums.getLast().tracks.add(new Track());
            return parseFileHelper(tokenList, current_token + 1, albums, inAlbum, true);
        } else if (tokenList.get(current_token).equals("/track")) {
            return parseFileHelper(tokenList, current_token + 1, albums, inAlbum, false);
        } else if (tokenList.get(current_token).equals("length")) {
            albums.getLast().tracks.getLast().length = tokenList.get(current_token + 1);
            return parseFileHelper(tokenList, current_token + 3, albums, inAlbum, inTrack);
        } else if (tokenList.get(current_token).equals("rating")) {
            albums.getLast().tracks.getLast().rating = Integer.parseInt(tokenList.get(current_token + 1));
            return parseFileHelper(tokenList, current_token + 3, albums, inAlbum, inTrack);
        } else if (tokenList.get(current_token).equals("feature")) {
            albums.getLast().tracks.getLast().features.add(tokenList.get(current_token + 1));
            return parseFileHelper(tokenList, current_token + 3, albums, inAlbum, inTrack);
        } else if (tokenList.get(current_token).equals("writing")) {
            albums.getLast().tracks.getLast().writers.add(tokenList.get(current_token + 1));
            return parseFileHelper(tokenList, current_token + 3, albums, inAlbum, inTrack);
        } else {
            return parseFileHelper(tokenList, current_token + 1, albums, inAlbum, inTrack);
        }
    }
}
