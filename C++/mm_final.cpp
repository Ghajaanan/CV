#include <iostream>
#include <cstdlib>
#include <ctime>
#include <vector>
#include <cmath>

/// functions for random number generation, do not alter the declarations
void set_random_seed();
int randn(int n);

struct mm_code_maker{

    void init(int i_length, int i_num){
        length = i_length;
        num = i_num;
    }

    void generate_sequence(){
        for(int i = 0; i < length; i++){
            sequence.push_back(randn(num));
        }
    }

    void give_feedback(const std::vector<int>& attempt, int& black_hits, int& white_hits){
        black_hits = 0;
        white_hits = 0;
        for (int i = 0; i < length; i++){ //This bit works out how many black hits.
            if(attempt[i] == sequence[i]){
                black_hits++;
            }
        }
        std::vector<int> attempt_count, sequence_count, min_count; //Makes three vectors with number of entries equal to number of possible symbols and all entries set to 0.
        for (int i = 0; i < num; i++){
            attempt_count.push_back(0);
            sequence_count.push_back(0);
            min_count.push_back(0);
        }
        for (int i = 0; i < length; i++){ //Keeps track of how many times each symbol appears in attempt or sequence
            attempt_count[attempt[i]]++;
            sequence_count[sequence[i]]++;
        }
        for (int i = 0; i < num; i++){ //Takes the smallest values from attempt_count and sequence_count and stores in min_count
            if(attempt_count[i] < sequence_count[i]){
                min_count[i] = attempt_count[i];
            }
            else{
                min_count[i] = sequence_count[i];
            }
        }
        int total_hits = 0; //Calculates total hits
        for(int i = 0; i < min_count.size(); i++){
            total_hits = total_hits + min_count[i];
        }
        white_hits = total_hits - black_hits;
    }

    std::vector<int> sequence; ///Don't alter/add member data. Add member functions if you want.
    int length;
    int num;

};


struct mm_solver{

    std::vector<std::vector<int> > set_s;
    std::vector<std::vector<int> > set_t;
    std::vector<int> code;
    int length;
    int num;
    bool first_attempt = true;

    void init(int i_length, int i_num){
        length = i_length;
        num = i_num;

        /// you can include additional implementation lines here if needed
    }

    void create_attempt(std::vector<int>& attempt){
        if (num + length <= 10){
            if(first_attempt){//create set_s and set_t
                //std::vector<int> code;
                std::vector<int> base_vector;
                //init_code(code, length);
                init_code(attempt, length);
                int counter = 0;
                init_code(code, length);
                init_set(set_t, code, length, num, counter);
                for(int i = 0; i < set_t.size(); i++){
                    set_s.push_back(set_t[i]);
                    if(set_s.size() == 1){
                        attempt = set_s[0];
                    }
                }
                min_max(set_s, set_t, attempt);
                first_attempt = false;
            }
            else{
                min_max(set_s, set_t, attempt);
                if(set_s.size() == 1){
                    attempt = set_s[0];
                }
            }
        }
        if(num + length >= 18){
            if(first_attempt){
                for (int i = 0; i < length; i++){
                attempt.push_back(-1);
                }
                first_attempt = false;
            }
            else{

            }
        }
        else{
            if(first_attempt){
                //std::vector<int> code;
                std::vector<int> base_vector;
                //init_code(code, length);
                init_code(attempt, length);
                int counter = 0;
                init_set(set_s, code, length, num, counter);
                attempt = set_s[rand()%set_s.size()];
                if(set_s.size() == 1){
                    attempt = set_s[0];
                }
                first_attempt = false;
            }
            else{
                attempt = set_s[rand()%set_s.size()];
                if(set_s.size() == 1){
                    attempt = set_s[0];
                }
            }
        }
    }

    /// do not alter the function interface (name, parameter list, void return)
    void learn(std::vector<int>& attempt, int black_hits, int white_hits){
        if(num + length >= 18){
            attempt[black_hits]++;
        }
        else{
            std::vector<std::vector<int> > temp_vector;

            for(int i = 0; i < set_s.size(); i++){
                int new_black_hits = 0;
                int new_white_hits = 0;
                give_feedback2(attempt, set_s[i], new_black_hits, new_white_hits);
                if(black_hits == new_black_hits && white_hits == new_white_hits){
                    temp_vector.push_back(set_s[i]);
                }
            }
            set_s.clear();
            for(int i = 0; i < temp_vector.size(); i++){
                set_s.push_back(temp_vector[i]);
            }
            //std::cout << " set_s size = " << set_s.size() << std::endl;
            //print_set(set_s);
        }
    }

    void bwgrid(std::vector<std::vector<int> >& bwhits){
        std::vector<int> temp;
        init_code(temp, length+1);
        for(int i = 0; i < length+1; i++){
            bwhits.push_back(temp);
        }
    }

    void min_max(std::vector<std::vector<int> > set_s, std::vector<std::vector<int> > set_t, std::vector<int>& next_attempt){
        int minmax_min = set_s.size() * set_t.size();
        int minmax_index = 0;
        for(int i = 0; i < set_t.size(); i++){
            std::vector<std::vector<int> > bwhits;
            bwgrid(bwhits);
            for(int j = 0; j < set_s.size(); j++){
                give_feedback(set_t[i], set_s[j], bwhits);
            }
            int temp_max = 0;
            for(int k = 0; k < bwhits.size(); k++){//finds most common b&w hits combination
                for(int l = 0; l < bwhits.size(); l++){
                    if(bwhits[k][l] > temp_max){
                        temp_max = bwhits[k][l];
                    }
                }
            }
            if(minmax_min > temp_max){
                minmax_min = temp_max;
                minmax_index = i;
            }
        }
        for(int i = 0; i < length; i++){
            next_attempt[i] = set_t[minmax_index][i];
        }
}

    void give_feedback(std::vector<int> set_s_entry, std::vector<int> set_t_entry, std::vector<std::vector<int> >& bwhits){
        int black_hits = 0;
        int white_hits = 0;
        int hits = 0;
        for(int i = 0; i < length; i++){ //Counts black hits
            if(set_s_entry[i] == set_t_entry[i]){
                black_hits++;
            }
        }
        for(int i = 0; i < length; i++){//Calculates total hits
            bool whit = false;
            for(int j = 0; j < length && !whit; j++){
                if(set_s_entry[i] == set_t_entry[j]){
                    whit = true;
                    set_t_entry[j] = -1;
                    hits++;
                }
            }
        }
        white_hits = hits - black_hits;
        bwhits[black_hits][white_hits]++;
    }

    void give_feedback2(std::vector<int> set_s_entry, std::vector<int> set_t_entry, int& new_black_hits, int& new_white_hits){
        int new_hits = 0;
        for(int i = 0; i < length; i++){ //Counts black hits
            if(set_s_entry[i] == set_t_entry[i]){
                new_black_hits++;
            }
        }
        for(int i = 0; i < length; i++){//Calculates total hits
            bool new_whit = false;
            for(int j = 0; j < length && !new_whit; j++){
                if(set_s_entry[i] == set_t_entry[j]){
                    new_whit = true;
                    set_t_entry[j] = -1;
                    new_hits++;
                }
            }
        }
        new_white_hits = new_hits - new_black_hits;
    }

    void init_code(std::vector<int>& code, int length){
        for (int i = 0; i < length; i++){
            code.push_back(0);
        }
    }

    void init_set(std::vector<std::vector<int> >& code_vector,std::vector<int>& code, int length, int num, int counter){
        if(length + num <= 10)
            if(length == 0){
                code_vector.push_back(code);
            }
            else{
                for(int i = 0; i < num; i++){
                    code[counter] = i;
                    init_set(code_vector, code, length - 1, num, counter + 1);
                }
            }
        else{
            double max_dec_num = pow(num, length);
            int max_dec = max_dec_num + 0.4;
            std::vector<int> temp;
            for(int i = 0; i < max_dec; i++){
                base_convert(i, temp);
                set_s.push_back(temp);
            }
        }
    }

    void print_set(std::vector<std::vector<int> > code_vector){//testing only
        for(int i = 0; i < code_vector.size(); i++){
            for(int j = 0; j < code_vector[i].size(); j++){
                std::cout << code_vector[i][j] << " ";
            }
            std::cout << std::endl;
        }
    }

    void print_vector(std::vector<int> vector){
        for(int i = 0; i < vector.size(); i++){
            std::cout << vector[i] << " ";
        }
        std::cout << std::endl;
    }

    void base_convert(int dec_count, std::vector<int>& base_vector){
        base_vector.clear();
        for (int i = 0; i < length; i++){
            base_vector.push_back(dec_count%num);
            dec_count = dec_count/num;
        }
    }

    /// you may add other member functions and member data as needed
    /// (keep in mind the distinction between member function variables
    /// and member data of the struct)

};



int main(){
    /// write the code for the main here in order to test your functions
    int total_attempt_count = 0;
    for(int z = 0; z < 1; z++){
        int length = 8;
        int num = 8;
        int black_hits = 0;
        int white_hits =0;
        std::vector<int> attempt;

        mm_solver solver; //Tells solver the parameters of the code.
        solver.init(length, num);

        mm_code_maker maker; //Tells code-maker the parameters of the code.
        maker.init(length, num);
        maker.generate_sequence();


        std::cout << "The sequence is:" << std::endl; //Prints sequence
        for(int i = 0; i < length; i++){
            std::cout << maker.sequence[i] << " ";
        }
        int attempt_count = 0;
        std::cout << "Solving..." << std::endl;
        while(black_hits != length && attempt_count <= 100){
            solver.create_attempt(attempt);
            std::cout << "next attempt is ";
            solver.print_vector(attempt);
            maker.give_feedback(attempt, black_hits, white_hits);
            if(black_hits != length){
                solver.learn(attempt, black_hits, white_hits);
            }
            attempt_count ++;
            total_attempt_count ++;
        }
        std::cout << "total attempts = " << attempt_count << std::endl;


    }
    std::cout << "total attempt count = " << total_attempt_count << std::endl;
    return 0;
}

/// not a great implementation for set_random_seed and for randn;
/// if you are trying to get better results you may want to change
/// the implementation using C++11 features, see for instance
/// https://isocpp.org/files/papers/n3551.pdf
/// but don't change the interface/declaration of the functions

void set_random_seed(){
    std::srand(std::time(0));
}

int randn(int n){
    return std::rand() % n;
}

/// add here the implementation for any other functions you wish to define and use
