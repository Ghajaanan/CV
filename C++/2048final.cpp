#include <iostream>
#include <vector>
#include <cstdlib>
#include <string>
#include <fstream>
#include <cmath>

bool merge_num(std::vector<int>& v, int bi, int ei);
void rotate_anti_clock(std::vector<int>& v);
bool game_over(const std::vector<int>& v);
void print_grid(const std::vector<int>& v);
int rowcoltoi(int row, int col, int n);
bool merge_grid(std::vector<int>& v);
bool movement(char input, std::vector<int>& v);
void random(std::vector<int>& v);

int main(){

    std::vector<int> grid;
    std::string filename;
    int temp;
    std::cout << "Enter initial configuration file name:" << std::endl;
    std::cin >> filename;
    std::ifstream infile;
    infile.open(filename.c_str());
    if(!infile.is_open()){
        std::cout << "Invalid file." << std::endl;
        for(int i = 0; i < 15; i++){
            grid.push_back(0);
        }
        grid.push_back(2);
    }
    while(infile >> temp){
        grid.push_back(temp);
    }

    char input;
    print_grid(grid);
    while(!game_over(grid)){
        std::cin >> input;
        bool boy;
        std::cout << std::endl;
        if((boy = movement(input, grid)) == true){
            random(grid);
            print_grid(grid);
        }
    }
    std::cout << "Game over :( " << std::endl;
    return 0;

    }


bool merge_grid(std::vector<int>& v){
    int n = std::sqrt(v.size());
    std::vector<int> temp;
    for(int i = 0; i < v.size(); i++){
        temp.push_back(v[i]);
    }
    for(int row = 1; row < (n+1); row++){//Merges the grid to the left.
        merge_num(temp, ((row-1)*n), (row*n));
    }
    //check if v != temp
    for (int j = 0; j < temp.size(); j++){
        if(temp[j] != v[j]){
            v.clear();
            for (int i = 0; i < temp.size(); i++){
                v.push_back(temp[i]);
            }
            return true; //A change has been made
        }
    }
    //Reaches this stage is v = temp
    return false;
}

bool merge_num(std::vector<int>& v, int bi, int ei){
    std::vector<int> temp1;//Stores bi to ei without zeroes
    std::vector<int> temp2;//Stores bi to ei without zeroes and merged numbers
    std::vector<int> out;//Stores complete vector
    int zero = 0;//Keeps track of how many zeroes must be added between bi and ei
    //Copies numbers up to bi to out
    for(int i = 0; i < bi; i++){
        out.push_back(v[i]);
    }
    //Copies numbers between bi and ei that are not 0 to temp1
    for(int i = bi; i < ei; i++){
        if(v[i] != 0){
            temp1.push_back(v[i]);
        }
        else{
            zero++;
        }
    }
    //Copies temp1 to temp2 while merging numbers
    if(temp1.size() != 0){
        int hold = temp1[0];


    for(int i = 1; i < temp1.size(); i++){
        if(hold == temp1[i]){
            temp2.push_back(hold*2);
            hold = 0;
            zero++;
        }
        else{
            if(hold == 0){
                hold = temp1[i];
            }
            else{
                temp2.push_back(hold);
                hold = temp1[i];
            }
        }
    }
    if(hold != 0){
        temp2.push_back(hold);
    }
    }

    //Updates out with merged temp2
    for(int i = 0; i < temp2.size(); i++){
        out.push_back(temp2[i]);
    }
    //Updates out with the correct number of zeroes
    for(int i = 0; i < zero; i++){
        out.push_back(0);
    }
    //Copies the rest of the numbers to the out vector.
    for(int i = ei; i < v.size(); i++){
        out.push_back(v[i]);
    }
    //Checks to see if a change has been made by comparing v and out
    int check = 0;
    for(int i = bi; i < ei; i++){
        if(v[i] != out[i]){
            check = 1;
        }
    //Clears v and then copies out back to v
    v.clear();
    for(int i = 0; i < out.size(); i++){
        v.push_back(out[i]);
    }
    //Returns appropriate value depending on whether a change has been made
    if(check == 1){
        return true;
    }
    else{
        return false;
    }
    }
}

void rotate_anti_clock(std::vector<int>& v){
    std::vector<int> temp;
    int n = std::sqrt(v.size());//n is the length of the grid
for(int i = 0; i < n; i++){//Rotates the grid
        for(int j = 1; j < (n+1); j++){
            temp.push_back(v[j*n-i-1]);
        }
    }
    v.clear();//Clears v and copies temp back into v.
    for(int i = 0; i < temp.size(); i++){
        v.push_back(temp[i]);
    }
    }


void print_grid(const std::vector<int>& v){
    int n = std::sqrt(v.size());//Finds the length of the side of the grid
    for(int row = 0; row < n; row++){
        for(int col = 0; col < n; col++){
            std::cout << v[rowcoltoi(row,col,n)] << '\t';
        }
        std::cout << std::endl;
    }
    std::cout << std::endl;
}

int rowcoltoi(int row, int col, int n){
    return row*n+col;
}

bool game_over(const std::vector<int>& v){
    std::vector<int> temp;
    for(int i = 0; i < v.size(); i++){//Copies vector v to temp.
        temp.push_back(v[i]);
        if(v[i]==0){
            return false; //Grid has a zero in it, hence game is not over.
        }
    }
    //We only have to check 3 directions to confirm if game over or not.
    bool value = merge_grid(temp);//If merge temp is successful, returns true. Otherwise, false.
    if(value == true){
        return false;//Game is not over.
    }
    else{
        rotate_anti_clock(temp);//Rotate unchanged matrix.
        value = merge_grid(temp);//Check again to see if it can be merged.
    }
    if(value == true){
        return false;//Game is not over.
    }
    else{
        rotate_anti_clock(temp);//Rotate unchanged matrix.
        value = merge_grid(temp);//Check again to see if it can be merged.//Game over.
    }
    if(value == true){
        return false;
    }
    else{
        return true;//Game over.
    }
}

bool movement(char input, std::vector<int>& v){
    bool merged = false;
    if((input != 'a') && (input != 's') && (input != 'd')&& (input != 'w')){//If invalid key is entered, bool movement returns a false value.
        return false;
    }
    if(input == 'a'){
        if(merge_grid(v)==true){
            merged = true;
        }
    }
    if(input == 'w'){
        rotate_anti_clock(v);
        if(merge_grid(v)==true){
            merged = true;
        }
        rotate_anti_clock(v);
        rotate_anti_clock(v);
        rotate_anti_clock(v);
    }
    if(input == 'd'){
        rotate_anti_clock(v);
        rotate_anti_clock(v);
        if(merge_grid(v)==true){
            merged = true;
        }
        rotate_anti_clock(v);
        rotate_anti_clock(v);
    }
    if(input == 's'){
        rotate_anti_clock(v);
        rotate_anti_clock(v);
        rotate_anti_clock(v);
        if(merge_grid(v)==true){
            merged = true;
        }
        rotate_anti_clock(v);
    }
    return merged;
}

void random(std::vector<int>& v){
    std::vector<int> temp;
    for(int i = 0; i < v.size(); i++){//Copies all indices where there is a 0 in vector v to a new vector temp.
        if(v[i]==0){
            temp.push_back(i);
        }
    }
    if(temp.size()>0){//Replaces one of the 0s at random with a 2.
        int two = rand() % temp.size();
        v[temp[two]] = 2;
    }
}
